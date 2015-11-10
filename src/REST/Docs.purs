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
import REST.JSON

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

-- | Render a `Document` as a HTML string.
-- |
-- | The base URL for the running service should be provided in the first argument.
documentToMarkup :: forall eff any. String -> Docs eff any -> Markup
documentToMarkup baseURL (Docs (Document d) _) = do
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
  H.h2 $ text "API Tester"
  H.form ! A.id "tester" ! A.method (fromMaybe "GET" d.method) $ do
    for_ routeArgs   $ textBox "param"
    for_ d.queryArgs $ textBox "query"
    for_ d.headers   $ textBox "header"
    for_ d.request $ \req -> do
      H.div ! A.className "form-group" $ do
        H.label $ text "Request Body"
        H.textarea ! A.className "form-control" ! A.id "request" $ text $ prettyJSON req
    H.button ! A.type' "submit" ! A.className "btn btn-default" $ text "Submit"
  H.h3 $ text "API Response"
  H.pre $ H.code ! A.id "tester-response" $ text "No response"
  H.script ! A.type' "text/javascript" $ text javascriptContent
  where
  routePartToArg :: RoutePart -> Maybe Arg
  routePartToArg (MatchPart arg) = Just arg
  routePartToArg _ = Nothing

  bulletedList :: forall a. L.List a -> (a -> Markup) -> Markup
  bulletedList xs f = H.ul (for_ xs (H.li <<< f))

  title :: String
  title = maybe "" (<> " ") d.method <> route

  route :: String
  route = "/" <> intercalate "/" (map fromRoutePart d.route)
    where
    fromRoutePart (LiteralPart s) = s
    fromRoutePart (MatchPart (Arg a)) = ":" <> a.key

  renderArg :: Arg -> Markup
  renderArg (Arg a) = do
    H.code $ text a.key
    text $ " - " <> a.comments

  textBox :: String -> Arg -> Markup
  textBox pfx (Arg a) = do
    H.div ! A.className "form-group" $ do
      H.label $ text a.key
      H.input ! A.type' "text" ! A.className "form-control" ! A.id (pfx <> "-" <> a.key)

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
    url = baseURL <> "/" <> intercalate "/" (map fromRoutePart d.route) <> renderQuery d.queryArgs
      where
      fromRoutePart (LiteralPart s) = s
      fromRoutePart (MatchPart (Arg a)) = "{" <> a.key <> "}"

      renderQuery L.Nil = ""
      renderQuery qs = "?" <> intercalate "\&" (map (\(Arg a) -> a.key <> "={" <> a.key <> "}") qs)

  javascriptContent :: String
  javascriptContent = foldMap (<> "\n") $
    [ "var tester = document.getElementById('tester');"
    , "tester.onsubmit = function() {"
    , "  var xhr = new XMLHttpRequest();"
    , "  var getFormValue = function(pfx, key) {"
    , "    return document.getElementById(pfx + '-' + key).value;"
    , "  };"
    , "  var uri = '" <>
        baseURL <>
        "'" <>
        routePart <>
        queryPart <>
        ";"
    , "  xhr.open(tester.method, uri, true);"
    ] ++
    foldMap (\(Arg a) -> [ "  xhr.setRequestHeader('" <> a.key <> "', getFormValue('header', '" <> a.key <> "'));" ]) d.headers ++
    [ "  xhr.onreadystatechange = function() {"
    , "    if (xhr.readyState == 4) {"
    , "      document.getElementById('tester-response').innerText = xhr.responseText;"
    , "    }"
    , "  }"
    , maybe "  xhr.send();" (\_ -> "  xhr.send(document.getElementById('request').value);") d.request
    , "  return false;"
    , "};"
    ]
    where
    routePart :: String
    routePart = case d.route of
                  L.Nil -> ""
                  _ -> foldMap fromRoutePart d.route

    fromRoutePart :: RoutePart -> String
    fromRoutePart (LiteralPart s) = " + '/" <> s <> "'"
    fromRoutePart (MatchPart (Arg a)) = " + '/' + getFormValue('param', '" <> a.key <> "')"

    queryPart :: String
    queryPart = case d.queryArgs of
                  L.Nil -> ""
                  _ -> " + '?' + " <> intercalate " + '&' + " (map fromQueryArg d.queryArgs)

    fromQueryArg :: Arg -> String
    fromQueryArg (Arg a) = "'" <> a.key <> "=' + " <> "getFormValue('query', '" <> a.key <> "')"

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
  String ->
  f (Docs eff any) ->
  (Markup -> Markup) ->
  Int ->
  Eff (http :: Node.HTTP | eff) Unit ->
  Eff (http :: Node.HTTP | eff) Unit
serveDocs baseURL endpoints wrap = serve (L.Cons tocEndpoint (L.toList (map toServer endpoints)))
  where
  toServer :: Docs eff any -> Server eff (Eff (http :: Node.HTTP | eff) Unit)
  toServer s@(Docs _ server) = staticHtmlResponse (lit "endpoint" *> server $> wrap (documentToMarkup baseURL s))

  tocEndpoint :: Server eff (Eff (http :: Node.HTTP | eff) Unit)
  tocEndpoint = staticHtmlResponse (get $> wrap (generateTOC (map generateDocs (L.toList endpoints))))
