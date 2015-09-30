-- | This module defines different types of web service implementation.

module REST.Service
  ( AsForeign
  , asForeign
  , Example()
  , ServiceError(..)
  , Service(..)
  , ServiceInfo(..)
  , ServiceImpl()
  , JSON()
  , jsonService
  , htmlService
  , staticHTML
  , runService
  ) where

import Prelude

import Data.Maybe
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

instance arrayAsForeign :: (AsForeign a) => AsForeign (Array a) where
  asForeign = toForeign <<< map asForeign

-- | An example of a request or response.
type Example = Unit -> Foreign

-- | A type synonym for JSON strings.
type JSON = String

-- | An error - status code and message.
data ServiceError = ServiceError Int String

-- | A generic service.
data Service f eff = Service ServiceInfo (f (ServiceImpl eff))

-- | Information about a service, for documentation purposes.
newtype ServiceInfo = ServiceInfo
  { comments :: Comments
  , request  :: Maybe Example
  , response :: Maybe Example
  }

-- | An implementation of a service
type ServiceImpl eff = Node.Request -> Node.Response -> Eff (http :: Node.HTTP | eff) Unit

-- | Create a `Service` which reads a JSON structure from the request body, and writes a JSON structure
-- | to the response body.
jsonService :: forall f eff req res.
  (Functor f, IsForeign req, AsForeign res) =>
  Comments ->
  (f (req -> (Either ServiceError res -> Eff (http :: Node.HTTP | eff) Unit) -> Eff (http :: Node.HTTP | eff) Unit)) ->
  Service f eff
jsonService comments fimpl = Service serviceInfo (map toImpl fimpl)
  where
  serviceInfo = ServiceInfo { comments: comments, request: Nothing, response: Nothing }

  toImpl impl req res = do
    let requestStream = Node.requestAsStream req
    Node.setEncoding requestStream Node.UTF8
    bodyRef <- unsafeRunRef $ newRef ""
    Node.onData requestStream \s -> do
      unsafeRunRef $ modifyRef bodyRef (<> s)
    Node.onError requestStream do
      sendResponse res 500 "text/plain" "Internal server error"
    Node.onEnd requestStream do
      body <- unsafeRunRef $ readRef bodyRef
      case readJSON body of
        Right req -> impl req respond
        Left err -> sendResponse res 400 "text/plain" ("Bad request: " <> show err)
    where
    respond :: Either ServiceError res -> Eff (http :: Node.HTTP | eff) Unit
    respond (Left (ServiceError statusCode message)) = sendResponse res statusCode "text/plain" message
    respond (Right response) = sendResponse res 200 "application/json" $ unsafeStringify $ asForeign response

-- | Create a `Service` which renders HTML content.
htmlService :: forall f eff.
  (Functor f) =>
  Comments ->
  (f ((Markup -> Eff (http :: Node.HTTP | eff) Unit) -> Eff (http :: Node.HTTP | eff) Unit)) ->
  Service f eff
htmlService comments fimpl = Service serviceInfo (map toImpl fimpl)
  where
  serviceInfo = ServiceInfo { comments: comments, request: Nothing, response: Nothing }
  toImpl impl _ res = impl (sendResponse res 200 "text/html" <<< render)

-- | Serve static HTML in the response.
staticHTML :: forall f eff. (Functor f) => Comments -> f Markup -> Service f eff
staticHTML comments m = htmlService comments (map (#) m)

sendResponse :: forall eff. Node.Response -> Int -> String -> String -> Eff (http :: Node.HTTP | eff) Unit
sendResponse res code contentType message = do
  Node.setStatusCode res code
  Node.setHeader res "Content-Type" contentType
  let responseStream = Node.responseAsStream res
  Node.writeString responseStream Node.UTF8 message (return unit)
  Node.end responseStream (return unit)

-- | Run a `Service`.
runService :: forall f eff. Service f eff -> f (ServiceImpl eff)
runService (Service _ fimpl) = fimpl
