module Test.Main where

import Prelude

import Data.Maybe
import Data.Either
import Data.Functor (($>))
import Data.Foreign
import Data.Foreign.Class
import Data.String (toUpper, null)

import Control.Apply
import Control.Monad.Eff
import Control.Monad.Eff.Console

import qualified Node.HTTP        as Node
import qualified Node.Encoding    as Node
import qualified Node.Stream      as Node

import REST.Endpoint
import REST.Server
import REST.Docs

import qualified Text.Smolder.HTML                as H
import qualified Text.Smolder.HTML.Attributes     as A
import Text.Smolder.Markup (Markup(), (!), text)
import Text.Smolder.Renderer.String (render)

data Echo = Echo String

instance echoIsForeign :: IsForeign Echo where
  read f = Echo <$> readProp "text" f

instance echoAsForeign :: AsForeign Echo where
  asForeign (Echo text) = toForeign { text: text }

instance echoHasExample :: HasExample Echo where
  example = Echo "Hello, World!"

home :: forall e eff. (Endpoint e) => e (Eff (http :: Node.HTTP | eff) Unit)
home = worker <$> (get *> response)
  where
  worker res = sendResponse res 200 "text/plain" "Hello, world!"

echo :: forall e eff. (Endpoint e) => e (Eff (http :: Node.HTTP | eff) Unit)
echo = worker <$> (docs *> post *> lit "echo" *> optional shout) <*> jsonRequest <*> jsonResponse
  where
  worker :: Maybe String -> Echo -> (Echo -> Eff (http :: Node.HTTP | eff) Unit) -> Eff (http :: Node.HTTP | eff) Unit
  worker (Just h) (Echo s) k | not (null h) = k (Echo (toUpper s))
  worker _ e k = k e

  docs :: e Unit
  docs = comments "Echos the request body in the response body."

  shout :: forall e. (Endpoint e) => e String
  shout = header "X-Shout" "This header should be non-empty if the result should be capitalized."

endpoints :: forall e eff. (Endpoint e) => Array (e (Eff (http :: Node.HTTP | eff) Unit))
endpoints = [ home, echo ]

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
  log "The API tester is configured to send requests to localhost:9000/api."
  log "To avoid CORS issues in the browser, you should forward requests from port 9000 to 8080/8081 accordingly."
  serve endpoints 8080 do
    log "Listening on port 8080."
  serveDocs "http://localhost:9000/api" endpoints template 8081 do
    log "Serving docs on port 8081."
