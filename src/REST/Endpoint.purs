-- | This module defines the `Endpoint` class, which is used to specify
-- | REST endpoints, independent of their implementation.

module REST.Endpoint
   ( class AsForeign
   , asForeign
   , Example()
   , class HasExample
   , example
   , ServiceError(..)
   , class Endpoint
   , method
   , lit
   , match
   , query
   , header
   , request
   , response
   , jsonRequest
   , jsonResponse
   , optional
   , comments
   , Hint()
   , Comments()
   , get
   , post
   , put
   , delete
   , Source()
   , Sink()
   , htmlResponse
   , staticHtmlResponse
   , sendResponse
   ) where

import Prelude ((<<<), class Applicative, Unit, map, apply, bind, pure, unit)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (class IsForeign)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.List as L

import Node.Encoding  as Node
import Node.HTTP      as Node
import Node.Stream    as Node

import Text.Smolder.Markup (Markup())
import Text.Smolder.Renderer.String (render)

-- | A human-readable hint as to the meaning of a route argument.
-- |
-- | These should be rendered in documentation, e.g. `/foo/:bar/baz`
type Hint = String

-- | A comment string
type Comments = String

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

-- | A `Sink` receives a response.
type Sink eff res = res -> Eff (http :: Node.HTTP | eff) Unit

-- | A `Source` provides a request asynchronously.
type Source eff req = (req -> Eff (http :: Node.HTTP | eff) Unit) -> Eff (http :: Node.HTTP, err :: EXCEPTION | eff) Unit

-- | A service error - status code and message.
data ServiceError = ServiceError Int String

-- | The `Endpoint` class is used to create a _specification_ of a REST endpoint.
-- |
-- | Using the `Endpoint` class polymorphically allows multiple interpretations, such
-- | as documentation generation or a server implementation.
-- |
-- | For example:
-- |
-- |     myEndpoint :: forall e. (Endpoint e) => e String
-- |     myEndpoint = lit "product" *> match "id" "Product ID" <* lit "sales"
-- |
-- | will create a specification for an endpoint which matches routes of the form
-- | `/product/:id/sales`.
class (Applicative e) <= Endpoint e where
  method       :: String -> e Unit
  lit          :: String -> e Unit
  match        :: Hint -> Comments -> e String
  query        :: String -> Comments -> e (L.List String)
  header       :: String -> Comments -> e String
  request      :: e Node.Request
  response     :: e Node.Response
  jsonRequest  :: forall req eff. (HasExample req) => e (Source eff (Either ServiceError req))
  jsonResponse :: forall res eff. (HasExample res) => e (Sink eff res)
  optional     :: forall a. e a -> e (Maybe a)
  comments     :: String -> e Unit

-- | Specify a `GET` endpoint.
get :: forall e. (Endpoint e) => e Unit
get = method "GET"

-- | Specify a `POST` endpoint.
post :: forall e. (Endpoint e) => e Unit
post = method "POST"

-- | Specify a `PUT` endpoint.
put :: forall e. (Endpoint e) => e Unit
put = method "PUT"

-- | Specify a `DELETE` endpoint.
delete :: forall e. (Endpoint e) => e Unit
delete = method "DELETE"

-- | Create a `Service` which renders HTML content.
htmlResponse :: forall e a eff. (Endpoint e) => e (Sink eff (Markup a))
htmlResponse = map toClient response
  where
  toClient :: Node.Response -> Sink eff (Markup a)
  toClient res = sendResponse res 200 "text/html" <<< render

-- | Serve static HTML in the response body.
staticHtmlResponse :: forall e eff. (Endpoint e) => e (Markup _) -> e (Eff (http :: Node.HTTP | eff) Unit)
staticHtmlResponse = apply htmlResponse

-- | Send a basic response to the client, specifying a status code, status message and response body.
sendResponse :: forall eff. Node.Response -> Int -> String -> String -> Eff (http :: Node.HTTP | eff) Unit
sendResponse res code contentType message = do
  Node.setStatusCode res code
  Node.setHeader res "Content-Type" contentType
  let responseStream = Node.responseAsStream res
  Node.writeString responseStream Node.UTF8 message (pure unit)
  Node.end responseStream (pure unit)
