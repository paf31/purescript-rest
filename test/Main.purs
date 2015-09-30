module Test.Main where

import Prelude

import Data.Either
import Data.Functor (($>))
import Data.Foreign
import Data.Foreign.Class

import Control.Apply
import Control.Monad.Eff.Console

import qualified Node.HTTP        as Node
import qualified Node.Encoding    as Node
import qualified Node.Stream      as Node

import REST.Endpoint
import REST.Server
import REST.Service
import REST.Docs

import qualified Text.Smolder.HTML                as H
import qualified Text.Smolder.HTML.Attributes     as A
import Text.Smolder.Markup
import Text.Smolder.Renderer.String (render)

data Echo = Echo String

instance echoIsForeign :: IsForeign Echo where
  read f = Echo <$> readProp "text" f

instance echoAsForeign :: AsForeign Echo where
  asForeign (Echo text) = toForeign { text: text }

instance echoHasExample :: HasExample Echo where
  example = Echo "Hello, World!"

echo :: forall e eff. (Endpoint e) => Service e eff
echo = withRequest (post *> lit "echo" $> \(Echo text) k -> k (Right (Echo text)))
         # jsonResponse
         # jsonRequest
         # withComments "This service echoes the route argument in the response body."

endpoints :: forall e eff. (Endpoint e) => Array (Service e eff)
endpoints = [ echo ]

template :: Markup -> Markup
template body = do
  H.html ! A.lang "en" $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.title $ text "API Documentation"
      H.link ! A.rel "stylesheet" ! A.href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    H.body do
      H.div ! A.className "container" $ body

main = do
  serve endpoints 8080 do
    log "Listening on port 8080."
  serveDocs endpoints template 8081 do
    log "Serving docs on port 8081."
