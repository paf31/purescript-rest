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

##### Instances
``` purescript
instance arrayAsForeign :: (AsForeign a) => AsForeign (Array a)
```

#### `HasExample`

``` purescript
class (AsForeign a) <= HasExample a where
  example :: a
```

A type class for requests and responses which have examples.

#### `Example`

``` purescript
type Example = Unit -> Foreign
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
data Service f eff
  = Service Comments ServiceType (f (ServiceImpl eff))
```

A generic service.

#### `ServiceType`

``` purescript
data ServiceType
  = JsonService Example Example
  | HtmlService
  | AnyService
```

Enumerates different types of service.

It is useful to differentiate these for documentation purposes.

#### `ServiceImpl`

``` purescript
type ServiceImpl eff = Request -> Response -> Eff (http :: HTTP | eff) Unit
```

An implementation of a service

#### `jsonService`

``` purescript
jsonService :: forall f eff req res. (Functor f, HasExample req, HasExample res) => Comments -> f (req -> (Either ServiceError res -> Eff (http :: HTTP | eff) Unit) -> Eff (http :: HTTP | eff) Unit) -> Service f eff
```

Create a `Service` which reads a JSON structure from the request body, and writes a JSON structure
to the response body.

#### `htmlService`

``` purescript
htmlService :: forall f eff. (Functor f) => Comments -> f ((Markup -> Eff (http :: HTTP | eff) Unit) -> Eff (http :: HTTP | eff) Unit) -> Service f eff
```

Create a `Service` which renders HTML content.

#### `staticHTML`

``` purescript
staticHTML :: forall f eff. (Functor f) => Comments -> f Markup -> Service f eff
```

Serve static HTML in the response.

#### `anyService`

``` purescript
anyService :: forall f eff. Comments -> f (ServiceImpl eff) -> Service f eff
```

Create a service from a low-level request/response handler.

#### `runService`

``` purescript
runService :: forall f eff. Service f eff -> f (ServiceImpl eff)
```

Run a `Service`.


