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
Functor Server
Apply Server
Applicative Server
Endpoint Server
```

#### `serve`

``` purescript
serve :: forall f eff. (Foldable f) => Options -> f (Server (Eff (http :: HTTP | eff) Unit)) -> Int -> Eff (http :: HTTP | eff) Unit -> Eff (http :: HTTP | eff) Unit
```

Serve a set of endpoints on the specified port.


