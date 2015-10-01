-- | This module defines the `Endpoint` class, which is used to specify
-- | REST endpoints, independent of their implementation.

module REST.Endpoint
   ( AsForeign
   , asForeign
   , Example()
   , HasExample
   , example
   , ServiceError(..)
   , Endpoint
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
   , Client()
   , htmlResponse
   , staticHtmlResponse
   , sendResponse
   ) where

import Prelude

import Data.Maybe
import Data.Foreign
import Data.Foreign.Class

import Control.Monad.Eff

import qualified Data.List as L

import qualified Node.URL       as Node
import qualified Node.HTTP      as Node
import qualified Node.Stream    as Node
import qualified Node.Encoding  as Node

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

-- | A `Client` receives a typed response from a service.
type Client eff res = res -> Eff (http :: Node.HTTP | eff) Unit

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
  jsonRequest  :: forall req. (HasExample req) => e req
  jsonResponse :: forall res eff. (HasExample res) => e (Client eff res)
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
htmlResponse :: forall e eff. (Endpoint e) => e (Client eff Markup)
htmlResponse = map toClient response
  where
  toClient :: Node.Response -> Client eff Markup
  toClient res = sendResponse res 200 "text/html" <<< render

-- | Serve static HTML in the response body.
staticHtmlResponse :: forall e eff. (Endpoint e) => e Markup -> e (Eff (http :: Node.HTTP | eff) Unit)
staticHtmlResponse = apply htmlResponse

-- | Send a basic response to the client, specifying a status code, status message and response body.
sendResponse :: forall eff. Node.Response -> Int -> String -> String -> Eff (http :: Node.HTTP | eff) Unit
sendResponse res code contentType message = do
  Node.setStatusCode res code
  Node.setHeader res "Content-Type" contentType
  let responseStream = Node.responseAsStream res
  Node.writeString responseStream Node.UTF8 message (return unit)
  Node.end responseStream (return unit)
