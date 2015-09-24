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

#### `Endpoint`

``` purescript
class (Applicative e) <= Endpoint e where
  method :: String -> e Unit
  lit :: String -> e Unit
  match :: Hint -> Comments -> e String
  query :: String -> Comments -> e (List String)
  header :: String -> Comments -> e String
  optional :: forall a. e a -> e (Maybe a)
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


