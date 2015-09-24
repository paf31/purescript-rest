-- | This module defines the `Endpoint` class, which is used to specify
-- | REST endpoints, independent of their implementation.

module REST.Endpoint
   ( Endpoint
   , method
   , lit
   , match
   , query
   , header
   , optional
   , Hint()
   , Comments()
   , get
   , post
   , put
   , delete
   ) where

import Prelude

import Data.Maybe
import Data.Foreign.Class

import qualified Data.List as L

-- | A human-readable hint as to the meaning of a route argument.
-- |
-- | These should be rendered in documentation, e.g. `/foo/:bar/baz`
type Hint = String

-- | A comment string
type Comments = String

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
  method   :: String -> e Unit
  lit      :: String -> e Unit
  match    :: Hint -> Comments -> e String
  query    :: String -> Comments -> e (L.List String)
  header   :: String -> Comments -> e String
  optional :: forall a. e a -> e (Maybe a)

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
