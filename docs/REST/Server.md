## Module REST.Server

This module implements a server for an `Endpoint` using the Node HTTP API.

#### `Server`

``` purescript
data Server a
```

An implementation of a REST service.

The `Endpoint` instance for `Service` can be used to connect a specification to
a server implementation, with `serve`.

##### Instances
``` purescript
instance functorServer :: Functor Server
instance applyServer :: Apply Server
instance applicativeServer :: Applicative Server
instance endpointServer :: Endpoint Server
```

#### `serve`

``` purescript
serve :: forall f eff. (Foldable f) => f (Server (Service eff)) -> Int -> Eff (http :: HTTP | eff) Unit -> Eff (http :: HTTP | eff) Unit
```

Serve a set of endpoints on the specified port.


