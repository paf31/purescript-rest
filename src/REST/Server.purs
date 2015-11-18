-- | This module implements a server for an `Endpoint` using the Node HTTP API.

module REST.Server
  ( Server()
  , serve
  ) where

import Prelude

import Data.Maybe
import Data.Tuple
import Data.Either (Either(..), either)
import Data.Monoid
import Data.Nullable (toMaybe)
import Data.String (split, null)
import Data.Foreign (Foreign(), readString, readArray)
import Data.Foreign.Class (IsForeign, readJSON)
import Data.Foldable (Foldable)
import Data.Traversable (traverse)

import Control.Alt ((<|>))
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)

import REST.Endpoint
import REST.JSON

import Unsafe.Coerce (unsafeCoerce)

import qualified Node.URL       as Node
import qualified Node.HTTP      as Node
import qualified Node.Stream    as Node
import qualified Node.Encoding  as Node

import qualified Data.StrMap  as S
import qualified Data.List    as L

type ParsedRequest =
  { route       :: L.List String
  , method      :: String
  , query       :: S.StrMap (L.List String)
  , headers     :: S.StrMap String
  }

parseRequest :: Node.Request -> ParsedRequest
parseRequest req =
  let url   = Node.parse (Node.requestURL req)
      query = Node.parseQueryString (fromMaybe "" (toMaybe url.query))
  in { route:   L.filter (not <<< null) $ L.toList $ split "/" $ fromMaybe "" $ toMaybe url.pathname
     , query:   parseQueryObject query
     , method:  Node.requestMethod req
     , headers: Node.requestHeaders req
     }

parseQueryObject :: Node.Query -> S.StrMap (L.List String)
parseQueryObject = map readStrings <<< queryAsStrMap
  where
  queryAsStrMap :: Node.Query -> S.StrMap Foreign
  queryAsStrMap = unsafeCoerce

  readStrings :: Foreign -> L.List String
  readStrings f = either (const L.Nil) id $ map L.toList (readArray f >>= traverse readString) <|> (L.singleton <$> readString f)

-- | The result of parsing a request
data ServerResult a = ServerResult ParsedRequest (Either ServiceError a)

instance functorServerResult :: Functor ServerResult where
  map f (ServerResult r a) = ServerResult r (map f a)

-- | An implementation of a REST service.
-- |
-- | The `Endpoint` instance for `Service` can be used to connect a specification to
-- | a server implementation, with `serve`.
data Server a = Server (Node.Request -> Node.Response -> ParsedRequest -> Maybe (ServerResult a))

instance functorServer :: Functor Server where
  map f (Server s) = Server \req res r -> map (map f) (s req res r)

instance applyServer :: Apply Server where
  apply (Server f) (Server a) = Server \req res r0 ->
    case f req res r0 of
      Just (ServerResult r1 f') -> map (\(ServerResult r2 a') -> ServerResult r2 (apply f' a')) (a req res r1)
      Nothing -> Nothing

instance applicativeServer :: Applicative Server where
  pure a = Server \_ _ r -> Just (ServerResult r (Right a))

instance endpointServer :: Endpoint Server where
  method m   = Server \_ _ r -> Just (ServerResult r (if m == r.method then Right unit else Left (ServiceError 405 "Method not allowed")))
  lit s      = Server \_ _ r -> case r.route of
                                  L.Cons hd tl | s == hd -> Just (ServerResult (r { route = tl }) (Right unit))
                                  _ -> Nothing
  match _ _  = Server \_ _ r -> case r.route of
                                  L.Cons hd tl -> Just (ServerResult (r { route = tl }) (Right hd))
                                  _ -> Nothing
  query q _  = Server \_ _ r -> case S.lookup q r.query of
                                  Nothing -> Just (ServerResult r (Left (ServiceError 400 ("Missing required query parameter " <> show q))))
                                  Just a -> Just (ServerResult r (Right a))
  header h _ = Server \_ _ r -> case S.lookup (Data.String.toLower h) r.headers of
                                  Nothing -> Just (ServerResult r (Left (ServiceError 400 ("Missing required header " <> show h))))
                                  Just a -> Just (ServerResult r (Right a))
  request    = Server \req _ r -> Just (ServerResult r (Right req))
  response   = Server \_ res r -> Just (ServerResult r (Right res))
  jsonRequest = Server \req res r ->
    let receive respond = do
          let requestStream = Node.requestAsStream req
          Node.setEncoding requestStream Node.UTF8
          bodyRef <- unsafeRunRef $ newRef ""
          Node.onData requestStream \s -> do
            unsafeRunRef $ modifyRef bodyRef (<> s)
          Node.onError requestStream do
            respond (Left (ServiceError 500 "Internal server error"))
          Node.onEnd requestStream do
            body <- unsafeRunRef $ readRef bodyRef
            case readJSON body of
              Right a -> respond (Right a)
              Left _ -> respond (Left (ServiceError 400 "Bad request"))
    in Just (ServerResult r (Right receive))
  jsonResponse = Server \req res r ->
    let respond = sendResponse res 200 "application/json" <<< prettyJSON <<< asForeign
    in Just (ServerResult r (Right respond))
  optional (Server s) = Server \req res r -> Just $
                          case s req res r of
                            Just (ServerResult r1 (Right a)) -> ServerResult r1 (Right (Just a))
                            Just (ServerResult r1 _) -> ServerResult r1 (Right Nothing)
                            Nothing -> ServerResult r (Right Nothing)
  comments _ = pure unit

-- | Serve a set of endpoints on the specified port.
serve :: forall f eff.
  (Foldable f) =>
  f (Server (Eff (http :: Node.HTTP | eff) Unit)) ->
  Int ->
  Eff (http :: Node.HTTP | eff) Unit ->
  Eff (http :: Node.HTTP | eff) Unit
serve endpoints port callback = do
  server <- Node.createServer respond
  Node.listen server port callback
  where
  respond :: Node.Request -> Node.Response -> Eff (http :: Node.HTTP | eff) Unit
  respond req res = do
    let pr = parseRequest req
    case firstSuccess (L.mapMaybe (\(Server f) -> f req res pr >>= ensureEOL) (L.toList endpoints)) of
      Left (ServiceError code msg) -> sendResponse res code "text/plain" msg
      Right impl -> impl

    where
    -- Ensure all route parts were matched
    ensureEOL :: forall a. ServerResult a -> Maybe (Either ServiceError a)
    ensureEOL (ServerResult { route: L.Nil } e) = Just e
    ensureEOL _ = Nothing

    -- Try each endpoint in order
    firstSuccess :: forall a. L.List (Either ServiceError a) -> Either ServiceError a
    firstSuccess L.Nil = Left (ServiceError 404 "No matching endpoint")
    firstSuccess (L.Cons (Left err) L.Nil) = Left err
    firstSuccess (L.Cons (Left _) rest) = firstSuccess rest
    firstSuccess (L.Cons (Right a) _) = Right a
