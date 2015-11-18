module Test.Main where

import Prelude

import Data.Either
import Data.Foreign.Generic
import Data.String (toUpper)
import Data.Generic

import Control.Apply
import Control.Monad.Eff
import Control.Monad.Eff.Console

import qualified Node.HTTP        as Node

import REST.Endpoint
import REST.Server
import REST.Docs

import qualified Text.Smolder.HTML                as H
import qualified Text.Smolder.HTML.Attributes     as A
import Text.Smolder.Markup (Markup(), (!), text)

newtype Echo = Echo { message :: String }

runEcho :: Echo -> { message :: String }
runEcho (Echo s) = s

derive instance genericEcho :: Generic Echo

instance echoHasExample :: HasExample Echo where
  example = Echo { message: "Hello, World!" }

home :: forall e eff. (Endpoint e) => e (Eff (http :: Node.HTTP | eff) Unit)
home = worker <$> (get *> response)
  where
  worker res = sendResponse res 200 "text/plain" "Hello, world!"

echo :: forall e eff. (Endpoint e) => e (Eff (http :: Node.HTTP | eff) Unit)
echo = worker <$> (docs *> post *> lit "echo" *> shoutHeader) <*> jsonRequest <*> jsonResponse <*> response
  where
  worker :: String -> Source eff (Either ServiceError Echo) -> Sink eff Echo -> Node.Response -> Eff (http :: Node.HTTP | eff) Unit
  worker shout source sink res = do
    source \e ->
      case e of
        Left (ServiceError code msg) -> sendResponse res code "text/plain" msg
        Right (Echo s) -> sink <<< Echo $
          case shout of
            "yes" -> { message: toUpper s.message }
            _ -> s

  docs :: e Unit
  docs = comments "Echos the request body in the response body."

  shoutHeader :: (Endpoint e) => e String
  shoutHeader = header "X-Shout" "This header should equal 'yes' if the result should be capitalized."

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

jsonOpts :: Options
jsonOpts = defaultOptions { unwrapNewtypes = true }

main :: forall e. Eff ( http :: Node.HTTP
                      , console :: CONSOLE
                      | e
                      )
                      Unit
main = do
  log "The API tester is configured to send requests to localhost:9000/api."
  log "To avoid CORS issues in the browser, you should forward requests from port 9000 to 8080/8081 accordingly."
  serve jsonOpts endpoints 8080 do
    log "Listening on port 8080."
  serveDocs jsonOpts "http://localhost:9000/api" endpoints template 8081 do
    log "Serving docs on port 8081."
