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
Semigroup Document
Monoid Document
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
documentToMarkup :: forall any. String -> Docs any -> Markup
```

Render a `Document` as a HTML string.

The base URL for the running service should be provided in the first argument.

#### `Docs`

``` purescript
data Docs a
```

Documentation for a REST service.

The `Endpoint` instance for `Docs` can be used to generate documentation
for a specification, using `generateDocs`, or `serveDocs`.

##### Instances
``` purescript
Functor Docs
Apply Docs
Applicative Docs
Endpoint Docs
```

#### `generateDocs`

``` purescript
generateDocs :: forall a. Docs a -> Document
```

Generate documentation for an `Endpoint` specification.

#### `serveDocs`

``` purescript
serveDocs :: forall f eff any. (Functor f, Foldable f) => String -> f (Docs any) -> (Markup -> Markup) -> Int -> Eff (http :: HTTP | eff) Unit -> Eff (http :: HTTP | eff) Unit
```

Serve documentation for a set of `Endpoint` specifications on the specified port.


