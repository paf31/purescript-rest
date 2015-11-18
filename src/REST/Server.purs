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

-- | An implementation of a REST service.
-- |
-- | The `Endpoint` instance for `Service` can be used to connect a specification to
-- | a server implementation, with `serve`.
data Server a = Server (Node.Request -> Node.Response -> ParsedRequest -> Maybe (Either ServiceError (Tuple ParsedRequest a)))

instance functorServer :: Functor Server where
  map f (Server s) = Server \req res r -> map (map (map f)) (s req res r)

instance applyServer :: Apply Server where
  apply (Server f) (Server a) = Server \req res r0 ->
    case f req res r0 of
      Just (Left err) -> Just (Left err)
      Just (Right (Tuple r1 f')) -> map (map (map f')) (a req res r1)
      Nothing -> Nothing

instance applicativeServer :: Applicative Server where
  pure a = Server \_ _ r -> Just (Right (Tuple r a))

instance endpointServer :: Endpoint Server where
  method m   = Server \_ _ r -> if m == r.method
                                  then Just (Right (Tuple r unit))
                                  else Nothing
  lit s      = Server \_ _ r -> case r.route of
                                  L.Cons hd tl | s == hd -> Just (Right (Tuple (r { route = tl }) unit))
                                  _ -> Nothing
  match _ _  = Server \_ _ r -> case r.route of
                                  L.Cons hd tl -> Just (Right (Tuple (r { route = tl }) hd))
                                  _ -> Nothing
  query q _  = Server \_ _ r -> map (Right <<< Tuple r) (S.lookup q r.query)
  header h _ = Server \_ _ r -> map (Right <<< Tuple r) (S.lookup (Data.String.toLower h) r.headers)
  request    = Server \req _ r -> Just $ Right (Tuple r req)
  response   = Server \_ res r -> Just $ Right (Tuple r res)
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
    in Just (Right (Tuple r receive))
  jsonResponse = Server \req res r ->
    let respond = sendResponse res 200 "application/json" <<< prettyJSON <<< asForeign
    in Just (Right (Tuple r respond))
  optional (Server s) = Server \req res r ->
                          case s req res r of
                            Nothing -> Just (Right (Tuple r Nothing))
                            Just e -> Just (map (map Just) e)
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
  respond req res = try (parseRequest req) (L.toList endpoints)
    where
    -- Ensure all route parts were matched
    ensureEOL :: forall a. Tuple ParsedRequest a -> Maybe a
    ensureEOL (Tuple { route: L.Nil } a) = return a
    ensureEOL _ = Nothing

    -- Try each endpoint in order
    try :: ParsedRequest ->
           L.List (Server (Eff (http :: Node.HTTP | eff) Unit)) ->
           Eff (http :: Node.HTTP | eff) Unit
    try _ L.Nil = sendResponse res 404 "text/plain" "No matching endpoint"
    try preq (L.Cons (Server s) rest) =
      case s req res preq of
        Nothing -> try preq rest
        Just (Left (ServiceError code msg)) -> sendResponse res code "text/plain" msg
        Just (Right impl) ->
          case ensureEOL impl of
            Nothing -> try preq rest
            Just impl -> impl
