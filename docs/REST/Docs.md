## Module REST.Docs

This module defines functions for generating and serving module documentation
for an `Endpoint` specification.

#### `Document`

``` purescript
newtype Document
  = Document { comments :: Maybe Comments, method :: Maybe String, route :: List RoutePart, queryArgs :: List Arg, headers :: List Arg, request :: Maybe Example, response :: Maybe Example }
```

The documentation data structure.

A `Document` can be generated from an `Endpoint` specification using `generateDocs`.

##### Instances
``` purescript
instance semigroupDocument :: Semigroup Document
instance monoidDocument :: Monoid Document
```

#### `RoutePart`

``` purescript
data RoutePart
  = LiteralPart String
  | MatchPart Arg
```

A `RoutePart` represents part of an endpoint route.

#### `Arg`

``` purescript
newtype Arg
  = Arg { key :: String, comments :: Comments }
```

An `Arg` represents an argument matched by a query argument or header.

#### `documentToMarkup`

``` purescript
documentToMarkup :: forall eff any. Docs eff any -> Markup
```

Render a `Document` as a HTML string.

#### `Docs`

``` purescript
data Docs eff a
```

Documentation for a REST service.

The `Endpoint` instance for `Docs` can be used to generate documentation
for a specification, using `generateDocs`, or `serveDocs`.

##### Instances
``` purescript
instance functorDocs :: Functor (Docs eff)
instance applyDocs :: Apply (Docs eff)
instance applicativeDocs :: Applicative (Docs eff)
instance endpointDocs :: Endpoint (Docs eff)
```

#### `generateDocs`

``` purescript
generateDocs :: forall eff a. Docs eff a -> Document
```

Generate documentation for an `Endpoint` specification.

#### `serveDocs`

``` purescript
serveDocs :: forall f a eff any. (Functor f, Foldable f) => f (Docs eff any) -> (Markup -> Markup) -> Int -> Eff (http :: HTTP | eff) Unit -> Eff (http :: HTTP | eff) Unit
```

Serve documentation for a set of `Endpoint` specifications on the specified port.


