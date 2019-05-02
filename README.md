# servant-cli

Parse command line arguments into a servant client, from a servant API.

Mainly used through `parseClient` and `parseHandleClient`. `parseClient`
returns a servant client action that returns nested `Either`s for every
endpoint, but `parseHandleClient` allows you to conveniently specify how you
want to combine each endpoint entry into a single result.

Hooks into the annotation system used by *servant-docs* to provide descriptions
for parameters and captures.

See `example/greet.hs` for a sample program.
