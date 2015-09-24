-- | This module defines functions for generating and serving module documentation
-- | for an `Endpoint` specification.

module REST.Docs
  ( Document(..)
  , RoutePart(..)
  , Arg(..)
  , documentToHTML
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
import Data.Foldable (Foldable, foldMap, for_)

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

-- | Render a `Document` as a HTML string.
documentToHTML :: forall eff. Document -> Service eff -> Markup
documentToHTML (Document d) service = do
  H.h1 $ text title
  H.p $ text $ serviceNotes service
  H.h2 $ text "Route Parameters"
  bulletedList (L.mapMaybe routePartToArg d.route) renderArg
  H.h2 $ text "Query Parameters"
  bulletedList d.queryArgs renderArg
  H.h2 $ text "Headers"
  bulletedList d.headers renderArg
  serviceExtra service
  where
  routePartToArg :: RoutePart -> Maybe Arg
  routePartToArg (MatchPart arg) = Just arg
  routePartToArg _ = Nothing

  bulletedList :: forall a. L.List a -> (a -> Markup) -> Markup
  bulletedList L.Nil _ = H.p ! A.className "text-muted" $ text "None"
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

  serviceNotes :: Service eff -> String
  serviceNotes (JsonService notes _ _ _) = notes
  serviceNotes (HtmlService notes _) = notes
  serviceNotes (AnyService notes _) = notes

  serviceExtra :: Service eff -> Markup
  serviceExtra (JsonService _ req res _) = do
    H.h2 $ text "Request Body"
    H.pre $ H.code $ text $ req unit
    H.h2 $ text "Response Body"
    H.pre $ H.code $ text $ res unit
  serviceExtra _ = mempty

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
data Docs a = Docs Document (Server Unit) a

instance functorDocs :: Functor Docs where
  map f (Docs d s a) = Docs d s (f a)

instance applyDocs :: Apply Docs where
  apply (Docs d1 s1 f) (Docs d2 s2 a) = Docs (d1 <> d2) (s1 *> s2) (f a)

instance applicativeDocs :: Applicative Docs where
  pure a = Docs mempty (pure unit) a

instance endpointDocs :: Endpoint Docs where
  method m            = Docs (Document (emptyDoc { method = Just m }))
                             (pure unit) unit
  lit s               = Docs (Document (emptyDoc { route = L.singleton (LiteralPart s) }))
                             (lit s) unit
  match hint comments = Docs (Document (emptyDoc { route = L.singleton (MatchPart (Arg { key: hint, comments: comments })) }))
                             (lit "_") ""
  query key comments  = Docs (Document (emptyDoc { queryArgs = L.singleton (Arg { key: key, comments: comments }) }))
                             (pure unit) L.Nil
  header key comments = Docs (Document (emptyDoc { headers = L.singleton (Arg { key: key, comments: comments }) }))
                             (pure unit) ""
  optional = map Just

-- | Generate documentation for an `Endpoint` specification.
generateDocs :: forall a. Docs a -> Document
generateDocs (Docs d _ _) = d

-- | Serve documentation for a set of `Endpoint` specifications on the specified port.
serveDocs :: forall f a eff any.
  (Functor f, Foldable f) =>
  f (Docs (Service any)) ->
  (Markup -> Markup) ->
  Int ->
  Eff (http :: Node.HTTP | eff) Unit ->
  Eff (http :: Node.HTTP | eff) Unit
serveDocs endpoints wrap = serve (L.Cons tocEndpoint (L.toList (map toServer endpoints)))
  where
  toServer :: Docs (Service any) -> Server (Service eff)
  toServer (Docs d s service) = lit "endpoint" *> s $> toApplication d service

  toApplication :: Document -> Service any -> Service eff
  toApplication docs service = staticHTML "Endpoint" $ wrap $ documentToHTML docs service

  tocEndpoint :: Server (Service eff)
  tocEndpoint = get $> staticHTML "Table of Contents" (wrap (generateTOC (map generateDocs (L.toList endpoints))))
