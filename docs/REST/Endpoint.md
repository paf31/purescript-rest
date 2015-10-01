## Module REST.Endpoint

This module defines the `Endpoint` class, which is used to specify
REST endpoints, independent of their implementation.

#### `Hint`

``` purescript
type Hint = String
```

A human-readable hint as to the meaning of a route argument.

These should be rendered in documentation, e.g. `/foo/:bar/baz`

#### `Comments`

``` purescript
type Comments = String
```

A comment string

#### `AsForeign`

``` purescript
class (IsForeign a) <= AsForeign a where
  asForeign :: a -> Foreign
```

The `AsForeign` class extends `IsForeign` so that data types can be _serialized_ back to
foreign values.

`read` and `asForeign` should be almost-inverse:

- `read <<< asForeign = pure`
- `read <<< asForeign <=< read = read`

##### Instances
``` purescript
instance arrayAsForeign :: (AsForeign a) => AsForeign (Array a)
```

#### `Example`

``` purescript
type Example = Foreign
```

An example of a request or response.

#### `HasExample`

``` purescript
class (AsForeign a) <= HasExample a where
  example :: a
```

A class for types which have examples.

#### `Client`

``` purescript
type Client eff res = res -> Eff (http :: HTTP | eff) Unit
```

A `Client` receives a typed response from a service.

#### `ServiceError`

``` purescript
data ServiceError
  = ServiceError Int String
```

A service error - status code and message.

#### `Endpoint`

``` purescript
class (Applicative e) <= Endpoint e where
  method :: String -> e Unit
  lit :: String -> e Unit
  match :: Hint -> Comments -> e String
  query :: String -> Comments -> e (List String)
  header :: String -> Comments -> e String
  request :: e Request
  response :: e Response
  jsonRequest :: forall req. (HasExample req) => e req
  jsonResponse :: forall res eff. (HasExample res) => e (Client eff res)
  optional :: forall a. e a -> e (Maybe a)
  comments :: String -> e Unit
```

The `Endpoint` class is used to create a _specification_ of a REST endpoint.

Using the `Endpoint` class polymorphically allows multiple interpretations, such
as documentation generation or a server implementation.

For example:

    myEndpoint :: forall e. (Endpoint e) => e String
    myEndpoint = lit "product" *> match "id" "Product ID" <* lit "sales"

will create a specification for an endpoint which matches routes of the form
`/product/:id/sales`.

#### `get`

``` purescript
get :: forall e. (Endpoint e) => e Unit
```

Specify a `GET` endpoint.

#### `post`

``` purescript
post :: forall e. (Endpoint e) => e Unit
```

Specify a `POST` endpoint.

#### `put`

``` purescript
put :: forall e. (Endpoint e) => e Unit
```

Specify a `PUT` endpoint.

#### `delete`

``` purescript
delete :: forall e. (Endpoint e) => e Unit
```

Specify a `DELETE` endpoint.

#### `htmlResponse`

``` purescript
htmlResponse :: forall e eff. (Endpoint e) => e (Client eff Markup)
```

Create a `Service` which renders HTML content.

#### `staticHtmlResponse`

``` purescript
staticHtmlResponse :: forall e eff. (Endpoint e) => e Markup -> e (Eff (http :: HTTP | eff) Unit)
```

Serve static HTML in the response body.

#### `sendResponse`

``` purescript
sendResponse :: forall eff. Response -> Int -> String -> String -> Eff (http :: HTTP | eff) Unit
```

Send a basic response to the client, specifying a status code, status message and response body.


