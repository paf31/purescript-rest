-- | This module defines different types of web service implementation.

module REST.Service
  ( AsForeign
  , asForeign
  , Example()
  , HasExample
  , example
  , ServiceError(..)
  , Service(..)
  , ServiceInfo(..)
  , ServiceImpl()
  , JSON()
  , withComments
  , WithRequest()
  , withRequest
  , jsonRequest
  , jsonResponse
  , htmlResponse
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
type Example = Foreign

-- | A class for types which have examples.
class (AsForeign a) <= HasExample a where
  example :: a

-- | A type synonym for JSON strings.
type JSON = String

-- | An error - status code and message.
data ServiceError = ServiceError Int String

-- | A generic service.
data Service f eff = Service ServiceInfo (f (ServiceImpl eff))

-- | Information about a service, for documentation purposes.
newtype ServiceInfo = ServiceInfo
  { comments :: Maybe Comments
  , request  :: Maybe Example
  , response :: Maybe Example
  }

-- | Add comments to a `Service`, for documentation purposes.
withComments :: forall f eff. Comments -> Service f eff -> Service f eff
withComments comments (Service (ServiceInfo serviceInfo) fs) = Service (ServiceInfo (serviceInfo { comments = Just comments })) fs

-- | An implementation of a service
type ServiceImpl eff = Node.Request -> Node.Response -> Eff (http :: Node.HTTP | eff) Unit

newtype WithRequest req f a = WithRequest (f (req -> a))

instance functorWithRequest :: (Functor f) => Functor (WithRequest req f) where
  map f (WithRequest fk) = WithRequest (map (>>> f) fk)

-- | Build a structure of type `WithRequest` to capture the request body.
withRequest :: forall req f a. f (req -> a) -> WithRequest req f a
withRequest = WithRequest

-- | Create a `Service` which parses a JSON request body.
-- |
-- | The `WithRequest` data structure is necessary so that the request is only available
-- | _after_ parsing the route.
jsonRequest :: forall f eff req.
  (Functor f, HasExample req) =>
  Service (WithRequest req f) eff ->
  Service f eff
jsonRequest (Service (ServiceInfo info) (WithRequest fimpl)) = Service serviceInfo (map toImpl fimpl)
  where
  serviceInfo :: ServiceInfo
  serviceInfo = ServiceInfo (info { request = Just (asForeign (example :: req)) })

  toImpl :: (req -> ServiceImpl eff) -> ServiceImpl eff
  toImpl f req res = do
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
        Right a -> f a req res
        Left err -> sendResponse res 400 "text/plain" ("Bad request: " <> show err)

-- | Create a `Service` which writes a JSON structure to the response body.
jsonResponse :: forall f eff res.
  (Functor f, HasExample res) =>
  f ((Either ServiceError res -> Eff (http :: Node.HTTP | eff) Unit) -> Eff (http :: Node.HTTP | eff) Unit) ->
  Service f eff
jsonResponse fimpl = Service serviceInfo (map toImpl fimpl)
  where
  serviceInfo :: ServiceInfo
  serviceInfo = ServiceInfo { comments: Nothing, request: Nothing, response: Just (asForeign (example :: res)) }

  toImpl :: ((Either ServiceError res -> Eff (http :: Node.HTTP | eff) Unit) -> Eff (http :: Node.HTTP | eff) Unit) -> ServiceImpl eff
  toImpl impl _ res = impl \result ->
    case result of
      Left (ServiceError statusCode message) -> sendResponse res statusCode "text/plain" message
      Right response -> sendResponse res 200 "application/json" $ unsafeStringify $ asForeign response

-- | Create a `Service` which renders HTML content.
htmlResponse :: forall f eff.
  (Functor f) =>
  (f ((Markup -> Eff (http :: Node.HTTP | eff) Unit) -> Eff (http :: Node.HTTP | eff) Unit)) ->
  Service f eff
htmlResponse fimpl = Service serviceInfo (map toImpl fimpl)
  where
  serviceInfo :: ServiceInfo
  serviceInfo = ServiceInfo { comments: Nothing, request: Nothing, response: Nothing }

  toImpl :: ((Markup -> Eff (http :: Node.HTTP | eff) Unit) -> Eff (http :: Node.HTTP | eff) Unit) -> ServiceImpl eff
  toImpl impl _ res = impl (sendResponse res 200 "text/html" <<< render)

-- | Serve static HTML in the response.
staticHTML :: forall f eff. (Functor f) => f Markup -> Service f eff
staticHTML m = htmlResponse (map (#) m)

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
