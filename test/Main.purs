module Test.Main where

import Prelude

import Control.Apply
import Control.Monad.Eff.Console

import qualified Node.HTTP        as Node
import qualified Node.Encoding    as Node
import qualified Node.Stream      as Node

import REST.Endpoint
import REST.Server
import REST.Docs

import qualified Text.Smolder.HTML                as H
import qualified Text.Smolder.HTML.Attributes     as A
import Text.Smolder.Markup
import Text.Smolder.Renderer.String (render)

echo :: forall eff. String -> Application eff
echo text _ res = do
  let outputStream = Node.responseAsStream res
  Node.setHeader res "Content-Type" "text/plain"
  Node.writeString outputStream Node.UTF8 text (return unit)
  Node.end outputStream (return unit)

endpoints :: forall e eff. (Endpoint e) => Array (e (Application eff))
endpoints =
  [ echo <$> (get *> lit "echo" *> match "text" "The string to echo")
  ]

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
