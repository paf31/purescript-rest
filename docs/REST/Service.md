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
  = Service ServiceInfo (f (ServiceImpl eff))
```

A generic service.

#### `ServiceInfo`

``` purescript
newtype ServiceInfo
  = ServiceInfo { comments :: Maybe Comments, request :: Maybe Example, response :: Maybe Example }
```

Information about a service, for documentation purposes.

#### `withComments`

``` purescript
withComments :: forall f eff. Comments -> Service f eff -> Service f eff
```

Add comments to a `Service`, for documentation purposes.

#### `ServiceImpl`

``` purescript
type ServiceImpl eff = Request -> Response -> Eff (http :: HTTP | eff) Unit
```

An implementation of a service

#### `With`

``` purescript
newtype With i f a
```

##### Instances
``` purescript
instance functorWith :: (Functor f) => Functor (With i f)
```

#### `with`

``` purescript
with :: forall i f a. f (i -> a) -> With i f a
```

Build a structure of type `With` to capture an argument in the service implementation.

#### `jsonRequest`

``` purescript
jsonRequest :: forall f eff req. (Functor f, HasExample req) => Service (With req f) eff -> Service f eff
```

Create a `Service` which parses a JSON request body.

The `WithRequest` data structure is necessary so that the request is only available
_after_ parsing the route.

#### `jsonResponse`

``` purescript
jsonResponse :: forall f eff res. (Functor f, HasExample res) => Service (With (Either ServiceError res -> Eff (http :: HTTP | eff) Unit) f) eff -> Service f eff
```

Create a `Service` which writes a JSON structure to the response body.

#### `htmlResponse`

``` purescript
htmlResponse :: forall f eff res. (Functor f) => Service (With (Markup -> Eff (http :: HTTP | eff) Unit) f) eff -> Service f eff
```

Create a `Service` which renders HTML content.

#### `genericService`

``` purescript
genericService :: forall f eff. f (ServiceImpl eff) -> Service f eff
```

Create a service from a generic request/response handler.

#### `simpleService`

``` purescript
simpleService :: forall f eff. (Functor f) => f (Eff (http :: HTTP | eff) Unit) -> Service f eff
```

Create a service from an effectful action.

#### `staticHTML`

``` purescript
staticHTML :: forall f eff. (Functor f) => f Markup -> Service f eff
```

Serve static HTML in the response.

#### `runService`

``` purescript
runService :: forall f eff. Service f eff -> f (ServiceImpl eff)
```

Run a `Service`.


