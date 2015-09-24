-- | This module defines different types of web service implementation.

module REST.Service
  ( AsForeign
  , asForeign
  , Example()
  , HasExample
  , example
  , ServiceError(..)
  , Service(..)
  , JSON()
  , jsonService
  , staticHTML
  , runService
  ) where

import Prelude

import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)

import Global.Unsafe (unsafeStringify)

import qualified Node.URL       as Node
import qualified Node.HTTP      as Node
import qualified Node.Stream    as Node
import qualified Node.Encoding  as Node

import Text.Smolder.Markup (Markup())
import Text.Smolder.Renderer.String (render)

import REST.Endpoint

-- | The `AsForeign` class extends `IsForeign` so that data types can be _serialized_ back to
-- | foreign values.
-- |
-- | `read` and `asForeign` should be almost-inverse:
-- |
-- | - `read <<< asForeign = pure`
-- | - `read <<< asForeign <=< read = read`
class (IsForeign a) <= AsForeign a where
  asForeign :: a -> Foreign

-- | A type class for requests and responses which have examples.
class (AsForeign a) <= HasExample a where
  example :: a

-- | An example of a request or response.
type Example = Unit -> JSON

-- | A type synonym for JSON strings.
type JSON = String

-- | An error - status code and message.
data ServiceError = ServiceError Int String

-- | Enumerates different types of service.
-- |
-- | It is useful to differentiate these for documentation purposes.
data Service eff
  = JsonService Comments Example Example (JSON -> (Either ServiceError JSON -> Eff (http :: Node.HTTP | eff) Unit) -> Eff (http :: Node.HTTP | eff) Unit)
  | HtmlService Comments ((Markup -> Eff (http :: Node.HTTP | eff) Unit) -> Eff (http :: Node.HTTP | eff) Unit)
  | AnyService  Comments (Node.Request -> Node.Response -> Eff (http :: Node.HTTP | eff) Unit)

-- | Create a `Service` which reads a JSON structure from the request body, and writes a JSON structure
-- | to the response body.
jsonService :: forall req res eff.
  (IsForeign req, AsForeign res, HasExample req, HasExample res) =>
  Comments ->
  (req -> (Either ServiceError res -> Eff (http :: Node.HTTP | eff) Unit) -> Eff (http :: Node.HTTP | eff) Unit) ->
  Service eff
jsonService comments f =
  JsonService comments
              (\_ -> unsafeStringify $ asForeign (example :: req))
              (\_ -> unsafeStringify $ asForeign (example :: res))
              convert
  where
  convert :: JSON -> (Either ServiceError JSON -> Eff (http :: Node.HTTP | eff) Unit) -> Eff (http :: Node.HTTP | eff) Unit
  convert fReq k =
    case readJSON fReq of
      Right req -> f req (k <<< map (unsafeStringify <<< asForeign))
      Left err -> k $ Left $ ServiceError 400 ("Bad request: " <> show err)

-- | Serve static HTML in the response.
staticHTML :: forall eff. Comments -> Markup -> Service eff
staticHTML comments m = HtmlService comments ($ m)

sendResponse :: forall eff. Node.Response -> Int -> String -> String -> Eff (http :: Node.HTTP | eff) Unit
sendResponse res code contentType message = do
  Node.setStatusCode res code
  Node.setHeader res "Content-Type" contentType
  let responseStream = Node.responseAsStream res
  Node.writeString responseStream Node.UTF8 message (return unit)
  Node.end responseStream (return unit)

-- | Run a `Service`.
runService :: forall eff. Service eff -> Node.Request -> Node.Response -> Eff (http :: Node.HTTP | eff) Unit
runService (JsonService _ _ _ f) req res = do
  let requestStream = Node.requestAsStream req
  Node.setEncoding requestStream Node.UTF8
  bodyRef <- unsafeRunRef $ newRef ""
  Node.onData requestStream \s -> do
    unsafeRunRef $ modifyRef bodyRef (<> s)
  Node.onError requestStream do
    sendResponse res 500 "text/plain" "Internal server error"
  Node.onEnd requestStream do
    body <- unsafeRunRef $ readRef bodyRef
    f body respond
  where
  respond :: Either ServiceError JSON -> Eff (http :: Node.HTTP | eff) Unit
  respond (Left (ServiceError statusCode message)) = sendResponse res statusCode "text/plain" message
  respond (Right json) = sendResponse res 200 "application/json" json
runService (HtmlService _ k) req res = k \markup -> sendResponse res 200 "text/html" (render markup)
runService (AnyService _ f) req res = f req res
