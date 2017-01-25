# purescript-rest

A toolkit for creating REST services with Node and PureScript.

- [Module Documentation](docs/REST/)
- [Example](test/Main.purs)

## Getting Started

The key functionality is in the `REST.Endpoint` module, which defines the `Endpoint` type class:

```purescript
class (Applicative e) <= Endpoint e
```

This type class can be used to parse routes, read query parameters and headers, read the request body using `purescript-foreign`, and get a callback with which to return a response.

This lets us define routes in an applicative style, as follows:

```purescript
endpoint :: forall e eff. (Endpoint e) => e (Eff (http :: Node.HTTP | eff) Unit)
endpoint = getProductDetails <$> (get *> lit "product" *> match "id" "Product ID") <*> lit "details"
```

Using a type class allows us to interpret our routes in different ways:

- As an actual web service
- As documentation, including an automatically-generated _API tester_.

See the [test project](test/Main.purs) for a worked example.  You can run the test
server by running `pulp test` in project root.
