# servant-cli

Parse command line arguments into a servant client, from a servant API, using
*optparse-applicative* for parsing, displaying help, and auto-completion.

Hooks into the annotation system used by *servant-docs* to provide descriptions
for parameters and captures.

See `example/greet.hs` for a sample program.

Getting started
---------------

Here's a sample API revolving around greeting and some deep paths, with
authentication.

```haskell
type TestApi =
        Summary "Send a greeting"
           :> "hello"
           :> Capture "name" Text
           :> QueryParam "capital" Bool
           :> Get '[JSON] Greet
   :<|> Summary "Greet utilities"
           :> "greet"
           :> ReqBody '[JSON] Greet
           :> ( Get  '[JSON] Int
           :<|> BasicAuth "login" Int
             :> Post '[JSON] NoContent
              )
   :<|> Summary "Deep paths test"
           :> "dig"
           :> "down"
           :> "deep"
           :> Summary "Almost there"
           :> Capture "name" Text
           :> "more"
           :> Summary "We made it"
           :> Get '[JSON] Text
```

