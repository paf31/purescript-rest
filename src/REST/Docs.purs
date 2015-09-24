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
import Data.Monoid
import Data.Functor (($>))
import Data.Foldable (Foldable, foldMap, for_)

import qualified Data.List as L

import REST.Endpoint
import REST.Server

import qualified Node.HTTP        as Node
import qualified Node.Encoding    as Node
import qualified Node.Stream      as Node

import Control.Alt ((<|>))
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
documentToHTML :: Document -> Markup
documentToHTML (Document d) = do
  H.h1 $ text title
  H.h2 $ text "Route Parameters"
  bulletedList (L.mapMaybe routePartToArg d.route) arg
  H.h2 $ text "Query Parameters"
  bulletedList d.queryArgs arg
  H.h2 $ text "Headers"
  bulletedList d.headers arg
  H.h2 $ text "Example"
  H.p $ text "TODO"
  where
  routePartToArg :: RoutePart -> Maybe Arg
  routePartToArg (MatchPart arg) = Just arg
  routePartToArg _ = Nothing

  bulletedList :: forall a. L.List a -> (a -> Markup) -> Markup
  bulletedList L.Nil _ = H.p ! A.className "text-muted" $ text "None"
  bulletedList xs f = H.ul (for_ xs (H.li <<< f))

  title :: String
  title = maybe "" id d.method <> " /api" <> foldMap fromRoutePart d.route
    where
    fromRoutePart (LiteralPart s) = "/" <> s
    fromRoutePart (MatchPart (Arg a)) = "/:" <> a.key

  arg :: Arg -> Markup
  arg (Arg a) = do
    H.code $ text a.key
    text $ " - " <> a.comments

-- | Documentation for a REST service.
-- |
-- | The `Endpoint` instance for `Docs` can be used to generate documentation
-- | for a specification, using `generateDocs`, or `serveDocs`.
newtype Docs e a = Docs (e Document)

instance functorDocs :: Functor (Docs e) where
  map _ (Docs d) = Docs d

instance applyDocs :: (Apply e) => Apply (Docs e) where
  apply (Docs f) (Docs a) = Docs (append <$> f <*> a)

instance applicativeDocs :: (Applicative e) => Applicative (Docs e) where
  pure _ = Docs (pure mempty)

instance endpointDocs :: (Endpoint e) => Endpoint (Docs e) where
  method m            = Docs $ pure                 $  Document (emptyDoc { method = Just m })
  lit s               = Docs $ lit s                $> Document (emptyDoc { route = L.singleton (LiteralPart s) })
  match hint comments = Docs $ match hint comments  $> Document (emptyDoc { route = L.singleton (MatchPart (Arg { key: hint, comments: comments })) })
  query key comments  = Docs $ pure                 $  Document (emptyDoc { queryArgs = L.singleton (Arg { key: key, comments: comments }) })
  header key comments = Docs $ pure                 $  Document (emptyDoc { headers = L.singleton (Arg { key: key, comments: comments }) })
  optional = map Just

-- | Generate documentation for an `Endpoint` specification.
generateDocs :: forall e a. (Endpoint e) => Docs e a -> e Document
generateDocs (Docs ed) = ed

-- | Serve documentation for a set of `Endpoint` specifications on the specified port.
serveDocs :: forall f a eff. (Functor f, Foldable f) => f (Docs Server a) -> (Markup -> Markup) -> Int -> Eff (http :: Node.HTTP | eff) Unit -> Eff (http :: Node.HTTP | eff) Unit
serveDocs endpoints wrap = serve (map (map toApplication <<< generateDocs) endpoints)
  where
  toApplication :: Document -> Application eff
  toApplication docs _ res = do
    let outputStream = Node.responseAsStream res
        html = render (wrap (documentToHTML docs))
    Node.setHeader res "Content-Type" "text/html"
    Node.writeString outputStream Node.UTF8 html (return unit)
    Node.end outputStream (return unit)
