-- | This module implements a server for an `Endpoint` using the Node HTTP API.

module REST.Server
  ( Server()
  , serve
  ) where

import Prelude

import Data.Maybe
import Data.Maybe.First (First(..), runFirst)
import Data.Tuple
import Data.Either (either)
import Data.Monoid
import Data.Nullable (toMaybe)
import Data.String (split, null)
import Data.Foreign (Foreign(), readString, readArray)
import Data.Foldable (Foldable, foldMap)
import Data.Traversable (traverse)

import Control.Alt ((<|>))
import Control.Monad.Eff

import REST.Endpoint
import REST.Service

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
  in { route:   L.filter (not <<< null) $ L.toList $ split "/" $ fromMaybe "" $ toMaybe url.path
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
data Server a = Server (ParsedRequest -> Maybe (Tuple ParsedRequest a))

instance functorServer :: Functor Server where
  map f (Server s) = Server (map (map f) <<< s)

instance applyServer :: Apply Server where
  apply (Server f) (Server a) = Server \r0 -> f r0 >>= \(Tuple r1 f') -> map (map f') (a r1)

instance applicativeServer :: Applicative Server where
  pure a = Server \r -> pure (Tuple r a)

instance endpointServer :: Endpoint Server where
  method m   = Server \r -> if m == r.method
                              then Just (Tuple r unit)
                              else Nothing
  lit s      = Server \r -> case r.route of
                              L.Cons hd tl | s == hd -> Just (Tuple (r { route = tl }) unit)
                              _ -> Nothing
  match _ _  = Server \r -> case r.route of
                              L.Cons hd tl -> Just (Tuple (r { route = tl }) hd)
                              _ -> Nothing
  query k _  = Server \r -> map (Tuple r) (S.lookup k r.query)
  header k _ = Server \r -> map (Tuple r) (S.lookup (Data.String.toLower k) r.headers)
  optional (Server s) = Server \r -> map Just <$> s r <|> pure (Tuple r Nothing)

-- | Serve a set of endpoints on the specified port.
serve :: forall f eff.
  (Foldable f) =>
  f (Service Server eff) ->
  Int ->
  Eff (http :: Node.HTTP | eff) Unit ->
  Eff (http :: Node.HTTP | eff) Unit
serve endpoints port callback = do
  server <- Node.createServer respond
  Node.listen server port callback
  where
  respond :: Node.Request -> Node.Response -> Eff (http :: Node.HTTP | eff) Unit
  respond req res = do
    let parsed = parseRequest req
        mimpl  = runFirst $ foldMap (\(Service _ _ (Server f)) -> First (f parsed >>= ensureEOL)) endpoints

        ensureEOL :: forall a. Tuple ParsedRequest a -> Maybe a
        ensureEOL (Tuple { route: L.Nil } a) = return a
        ensureEOL _ = Nothing
    case mimpl of
      Just impl -> impl req res
      _ -> do
        Node.setStatusCode res 404
        Node.setStatusMessage res "Not found"
        Node.setHeader res "Content-Type" "text/plain"
        let outputStream = Node.responseAsStream res
        Node.writeString outputStream Node.UTF8 "Not found" (return unit)
        Node.end outputStream (return unit)
