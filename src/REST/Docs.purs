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
import REST.Service

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
  { method    :: Maybe String
  , route     :: L.List RoutePart
  , queryArgs :: L.List Arg
  , headers   :: L.List Arg
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

emptyDoc :: { method    :: Maybe String
            , route     :: L.List RoutePart
            , queryArgs :: L.List Arg
            , headers   :: L.List Arg
            }
emptyDoc =
  { method:    Nothing
  , route:     mempty
  , queryArgs: mempty
  , headers:   mempty
  }

instance semigroupDocument :: Semigroup Document where
  append (Document d1) (Document d2) = Document
    { method:    d1.method    <|> d2.method
    , route:     d1.route     <>  d2.route
    , queryArgs: d1.queryArgs <>  d2.queryArgs
    , headers:   d1.headers   <>  d2.headers
    }

instance monoidDocument :: Monoid Document where
  mempty = Document emptyDoc

-- | Pretty-print JSON with spaces and new-lines
foreign import prettyJSON :: forall a. a -> JSON

-- | Render a `Document` as a HTML string.
documentToMarkup :: forall eff. Service Docs eff -> Markup
documentToMarkup (Service comments serviceType (Docs (Document d) _)) = do
  H.h1 $ text title
  H.p $ text comments
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
  renderRequestBody serviceType
  where
  routePartToArg :: RoutePart -> Maybe Arg
  routePartToArg (MatchPart arg) = Just arg
  routePartToArg _ = Nothing

  bulletedList :: forall a. L.List a -> (a -> Markup) -> Markup
  bulletedList xs f = H.ul (for_ xs (H.li <<< f))

  title :: String
  title = maybe "" (<> " ") d.method <> foldMap fromRoutePart d.route
    where
    fromRoutePart (LiteralPart s) = "/" <> s
    fromRoutePart (MatchPart (Arg a)) = "/:" <> a.key

  renderArg :: Arg -> Markup
  renderArg (Arg a) = do
    H.code $ text a.key
    text $ " - " <> a.comments

  cURLCommand :: String
  cURLCommand = intercalate "\\\n    " $ map (intercalate " ") $
    [ [ ">", "curl" ] ++ foldMap (\m -> [ "-X", m ]) d.method ] ++
    foldMap (\(Arg a) -> [ [ "-H", "'" ++ a.key ++ ": {" ++ a.key ++ "}'" ] ]) d.headers ++
    cURLRequestBody ++
    [ [ url ] ]
    where
    cURLRequestBody :: Array (Array String)
    cURLRequestBody =
      case serviceType of
        JsonService _ _ -> [ [ "-H", "'Content-Type: application/json'" ], ["-d", "@request.json", "-o", "response.json" ] ]
        _ -> []

    url :: String
    url = foldMap fromRoutePart d.route <> renderQuery d.queryArgs
      where
      fromRoutePart (LiteralPart s) = "/" <> s
      fromRoutePart (MatchPart (Arg a)) = "/{" <> a.key <> "}"

      renderQuery L.Nil = ""
      renderQuery qs = "?" <> intercalate "\&" (map (\(Arg a) -> a.key <> "={" <> a.key <> "}") qs)

  renderRequestBody :: ServiceType -> Markup
  renderRequestBody (JsonService req res) = do
    H.h3 $ text "request.json"
    H.pre $ H.code $ text $ prettyJSON $ req unit
    H.h3 $ H.strong $ text "response.json"
    H.pre $ H.code $ text $ prettyJSON $ res unit
  renderRequestBody _ = mempty

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
  routeToText = foldMap fromRoutePart
    where
    fromRoutePart (LiteralPart s) = "/" <> s
    fromRoutePart (MatchPart (Arg a)) = "/:" <> a.key

-- | Documentation for a REST service.
-- |
-- | The `Endpoint` instance for `Docs` can be used to generate documentation
-- | for a specification, using `generateDocs`, or `serveDocs`.
data Docs a = Docs Document (Server Unit)

instance functorDocs :: Functor Docs where
  map _ (Docs d s) = Docs d s

instance applyDocs :: Apply Docs where
  apply (Docs d1 s1) (Docs d2 s2) = Docs (d1 <> d2) (s1 *> s2)

instance applicativeDocs :: Applicative Docs where
  pure _ = Docs mempty (pure unit)

instance endpointDocs :: Endpoint Docs where
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
  optional = map Just

-- | Generate documentation for an `Endpoint` specification.
generateDocs :: forall eff. Service Docs eff -> Document
generateDocs (Service _ _ (Docs d _)) = d

-- | Serve documentation for a set of `Endpoint` specifications on the specified port.
serveDocs :: forall f a eff any.
  (Functor f, Foldable f) =>
  f (Service Docs any) ->
  (Markup -> Markup) ->
  Int ->
  Eff (http :: Node.HTTP | eff) Unit ->
  Eff (http :: Node.HTTP | eff) Unit
serveDocs endpoints wrap = serve (L.Cons tocEndpoint (L.toList (map toServer endpoints)))
  where
  toServer :: Service Docs any -> Service Server eff
  toServer s@(Service _ _ (Docs _ server)) = staticHTML "Endpoint" (lit "endpoint" *> server $> wrap (documentToMarkup s))

  tocEndpoint :: Service Server eff
  tocEndpoint = staticHTML "Table of Contents" (get $> wrap (generateTOC (map generateDocs (L.toList endpoints))))
