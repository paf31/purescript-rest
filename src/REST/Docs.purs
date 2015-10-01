-- | This module defines functions for generating and serving module documentation
-- | for an `Endpoint` specification.

module REST.Docs
  ( Document(..)
  , RoutePart(..)
  , Arg(..)
  , documentToMarkup
  , Docs()
  , generateDocs
  , serveDocs
  ) where

import Prelude

import Data.Maybe
import Data.Tuple
import Data.Monoid
import Data.Functor (($>))
import Data.Function (on)
import Data.Foldable (Foldable, foldMap, for_, intercalate)

import qualified Data.List as L

import REST.Endpoint
import REST.Server

import qualified Node.HTTP        as Node
import qualified Node.Encoding    as Node
import qualified Node.Stream      as Node
import qualified Node.URL         as Node

import Control.Alt ((<|>))
import Control.Apply
import Control.Monad (when)
import Control.Monad.Eff

import qualified Text.Smolder.HTML                as H
import qualified Text.Smolder.HTML.Attributes     as A
import Text.Smolder.Markup (Markup(), text, (!))
import Text.Smolder.Renderer.String (render)

-- | The documentation data structure.
-- |
-- | A `Document` can be generated from an `Endpoint` specification using `generateDocs`.
newtype Document = Document
  { comments  :: Maybe Comments
  , method    :: Maybe String
  , route     :: L.List RoutePart
  , queryArgs :: L.List Arg
  , headers   :: L.List Arg
  , request   :: Maybe Example
  , response  :: Maybe Example
  }

-- | A `RoutePart` represents part of an endpoint route.
data RoutePart
  = LiteralPart String
  | MatchPart Arg

-- | An `Arg` represents an argument matched by a query argument or header.
newtype Arg = Arg
  { key :: String
  , comments :: Comments
  }

emptyDoc ::
  { comments  :: Maybe Comments
  , method    :: Maybe String
  , request   :: Maybe Example
  , response  :: Maybe Example
  , route     :: L.List RoutePart
  , queryArgs :: L.List Arg
  , headers   :: L.List Arg
  }
emptyDoc =
  { comments:  Nothing
  , method:    Nothing
  , request:   Nothing
  , response:  Nothing
  , route:     mempty
  , queryArgs: mempty
  , headers:   mempty
  }

instance semigroupDocument :: Semigroup Document where
  append (Document d1) (Document d2) = Document
    { comments:  d1.comments  <|> d2.comments
    , method:    d1.method    <|> d2.method
    , request:   d1.request   <|> d2.request
    , response:  d1.response  <|> d2.response
    , route:     d1.route     <>  d2.route
    , queryArgs: d1.queryArgs <>  d2.queryArgs
    , headers:   d1.headers   <>  d2.headers
    }

instance monoidDocument :: Monoid Document where
  mempty = Document emptyDoc

-- | Pretty-print JSON with spaces and new-lines
foreign import prettyJSON :: forall a. a -> String

-- | Render a `Document` as a HTML string.
documentToMarkup :: forall eff any. Docs eff any -> Markup
documentToMarkup (Docs (Document d) _) = do
  H.h1 $ text title
  for_ d.comments (H.p <<< text)
  let routeArgs = L.mapMaybe routePartToArg d.route
  when (not $ L.null routeArgs) do
    H.h2 $ text "Route Parameters"
    bulletedList routeArgs renderArg
  when (not $ L.null d.queryArgs) do
    H.h2 $ text "Query Parameters"
    bulletedList d.queryArgs renderArg
  when (not $ L.null d.headers) do
    H.h2 $ text "Headers"
    bulletedList d.headers renderArg
  H.h2 $ text "Example Request"
  H.pre $ H.code $ text cURLCommand
  for_ d.request $ \req -> do
    H.h3 $ text "request.json"
    H.pre $ H.code $ text $ prettyJSON req
  for_ d.response $ \res -> do
    H.h3 $ text "response.json"
    H.pre $ H.code $ text $ prettyJSON res
  where
  routePartToArg :: RoutePart -> Maybe Arg
  routePartToArg (MatchPart arg) = Just arg
  routePartToArg _ = Nothing

  bulletedList :: forall a. L.List a -> (a -> Markup) -> Markup
  bulletedList xs f = H.ul (for_ xs (H.li <<< f))

  title :: String
  title = maybe "" (<> " ") d.method <> "/" <> intercalate "/" (map fromRoutePart d.route)
    where
    fromRoutePart (LiteralPart s) = s
    fromRoutePart (MatchPart (Arg a)) = ":" <> a.key

  renderArg :: Arg -> Markup
  renderArg (Arg a) = do
    H.code $ text a.key
    text $ " - " <> a.comments

  cURLCommand :: String
  cURLCommand = intercalate "\\\n    " $ map (intercalate " ") $
    [ [ ">", "curl" ] ++ foldMap (\m -> [ "-X", m ]) d.method ] ++
    foldMap (\(Arg a) -> [ [ "-H", "'" ++ a.key ++ ": {" ++ a.key ++ "}'" ] ]) d.headers ++
    cURLRequestBody ++
    cURLResponseBody ++
    [ [ url ] ]
    where
    cURLRequestBody :: Array (Array String)
    cURLRequestBody =
      case d.request of
        Just _ -> [ [ "-H", "'Content-Type: application/json'" ], [ "-d", "@request.json" ] ]
        _ -> []

    cURLResponseBody :: Array (Array String)
    cURLResponseBody =
      case d.response of
        Just _ -> [ [ "-o", "response.json" ] ]
        _ -> []

    url :: String
    url = "/" <> intercalate "/" (map fromRoutePart d.route) <> renderQuery d.queryArgs
      where
      fromRoutePart (LiteralPart s) = s
      fromRoutePart (MatchPart (Arg a)) = "{" <> a.key <> "}"

      renderQuery L.Nil = ""
      renderQuery qs = "?" <> intercalate "\&" (map (\(Arg a) -> a.key <> "={" <> a.key <> "}") qs)

generateTOC :: L.List Document -> Markup
generateTOC docs = do
  H.h1 $ text "API Documentation"
  H.ul $ foldMap toListItem $ L.sortBy (compare `on` fst) $ map toTuple docs
  where
  toTuple :: Document -> Tuple String String
  toTuple (Document d) = Tuple (routeToText d.route) (routeToHref d.route)

  toListItem :: Tuple String String -> Markup
  toListItem (Tuple s href) =
    H.li $ H.a ! A.href href $ text s

  routeToHref :: L.List RoutePart -> String
  routeToHref = Node.resolve "/" <<< ("/endpoint" <>) <<< foldMap fromRoutePart
    where
    fromRoutePart (LiteralPart s) = "/" <> s
    fromRoutePart (MatchPart _) = "/_"

  routeToText :: L.List RoutePart -> String
  routeToText = ("/" <>) <<< intercalate "/" <<< map fromRoutePart
    where
    fromRoutePart (LiteralPart s) = s
    fromRoutePart (MatchPart (Arg a)) = ":" <> a.key

-- | Documentation for a REST service.
-- |
-- | The `Endpoint` instance for `Docs` can be used to generate documentation
-- | for a specification, using `generateDocs`, or `serveDocs`.
data Docs eff a = Docs Document (Server eff Unit)

instance functorDocs :: Functor (Docs eff) where
  map _ (Docs d s) = Docs d s

instance applyDocs :: Apply (Docs eff) where
  apply (Docs d1 s1) (Docs d2 s2) = Docs (d1 <> d2) (s1 *> s2)

instance applicativeDocs :: Applicative (Docs eff) where
  pure _ = Docs mempty (pure unit)

instance endpointDocs :: Endpoint (Docs eff) where
  method m            = Docs (Document (emptyDoc { method = Just m }))
                             (pure unit)
  lit s               = Docs (Document (emptyDoc { route = L.singleton (LiteralPart s) }))
                             (lit s)
  match hint comments = Docs (Document (emptyDoc { route = L.singleton (MatchPart (Arg { key: hint, comments: comments })) }))
                             (lit "_")
  query key comments  = Docs (Document (emptyDoc { queryArgs = L.singleton (Arg { key: key, comments: comments }) }))
                             (pure unit)
  header key comments = Docs (Document (emptyDoc { headers = L.singleton (Arg { key: key, comments: comments }) }))
                             (pure unit)
  request             = Docs (Document emptyDoc) (pure unit)
  response            = Docs (Document emptyDoc) (pure unit)
  jsonRequest         = requestDocs
  jsonResponse        = responseDocs
  optional            = map Just
  comments s          = Docs (Document (emptyDoc { comments = Just s }))
                             (pure unit)

requestDocs :: forall eff req. (HasExample req) => Docs eff req
requestDocs = Docs (Document (emptyDoc { request = Just (asForeign (example :: req)) })) (pure unit)

responseDocs :: forall eff eff1 res. (HasExample res) => Docs eff (Client eff1 res)
responseDocs = Docs (Document (emptyDoc { response = Just (asForeign (example :: res)) })) (pure unit)

-- | Generate documentation for an `Endpoint` specification.
generateDocs :: forall eff a. Docs eff a -> Document
generateDocs (Docs d _) = d

-- | Serve documentation for a set of `Endpoint` specifications on the specified port.
serveDocs :: forall f a eff any.
  (Functor f, Foldable f) =>
  f (Docs eff any) ->
  (Markup -> Markup) ->
  Int ->
  Eff (http :: Node.HTTP | eff) Unit ->
  Eff (http :: Node.HTTP | eff) Unit
serveDocs endpoints wrap = serve (L.Cons tocEndpoint (L.toList (map toServer endpoints)))
  where
  toServer :: Docs eff any -> Server eff (Eff (http :: Node.HTTP | eff) Unit)
  toServer s@(Docs _ server) = staticHtmlResponse (lit "endpoint" *> server $> wrap (documentToMarkup s))

  tocEndpoint :: Server eff (Eff (http :: Node.HTTP | eff) Unit)
  tocEndpoint = staticHtmlResponse (get $> wrap (generateTOC (map generateDocs (L.toList endpoints))))
