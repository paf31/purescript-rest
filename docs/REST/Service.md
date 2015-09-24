## Module REST.Service

This module defines different types of web service implementation.

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

#### `HasExample`

``` purescript
class (AsForeign a) <= HasExample a where
  example :: a
```

A type class for requests and responses which have examples.

#### `Example`

``` purescript
type Example = Unit -> JSON
```

An example of a request or response.

#### `JSON`

``` purescript
type JSON = String
```

A type synonym for JSON strings.

#### `ServiceError`

``` purescript
data ServiceError
  = ServiceError Int String
```

An error - status code and message.

#### `Service`

``` purescript
data Service eff
  = JsonService Comments Example Example (JSON -> (Either ServiceError JSON -> Eff (http :: HTTP | eff) Unit) -> Eff (http :: HTTP | eff) Unit)
  | HtmlService Comments ((Markup -> Eff (http :: HTTP | eff) Unit) -> Eff (http :: HTTP | eff) Unit)
  | AnyService Comments (Request -> Response -> Eff (http :: HTTP | eff) Unit)
```

Enumerates different types of service.

It is useful to differentiate these for documentation purposes.

#### `jsonService`

``` purescript
jsonService :: forall req res eff. (IsForeign req, AsForeign res, HasExample req, HasExample res) => Comments -> (req -> (Either ServiceError res -> Eff (http :: HTTP | eff) Unit) -> Eff (http :: HTTP | eff) Unit) -> Service eff
```

Create a `Service` which reads a JSON structure from the request body, and writes a JSON structure
to the response body.

#### `staticHTML`

``` purescript
staticHTML :: forall eff. Comments -> Markup -> Service eff
```

Serve static HTML in the response.

#### `runService`

``` purescript
runService :: forall eff. Service eff -> Request -> Response -> Eff (http :: HTTP | eff) Unit
```

Run a `Service`.


