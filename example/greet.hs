{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

import           Data.Aeson
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text               (Text)
import           GHC.Generics
import           Options.Applicative
import           Servant.API
import           Servant.CLI
import           Servant.Client
import qualified Data.Text               as T

-- * Example

-- | A greet message data type
newtype Greet = Greet Text
  deriving (Generic, Show)

instance ParseBody Greet where
    parseBody = Greet <$> parseBody

-- | We can get JSON support automatically. This will be used to parse
-- and encode a Greeting as 'JSON'.
instance FromJSON Greet
instance ToJSON Greet

-- | We can also implement 'MimeRender' for additional formats like 'PlainText'.
instance MimeRender PlainText Greet where
    mimeRender Proxy (Greet s) = "\"" <> cs s <> "\""

-- We add some useful annotations to our captures,
-- query parameters and request body to make the docs
-- really helpful.
instance ToCapture (Capture "name" Text) where
  toCapture _ = DocCapture "name" "name of the person to greet"

instance ToCapture (Capture "greetid" Text) where
  toCapture _ = DocCapture "greetid" "identifier of the greet msg to remove"

instance ToParam (QueryParam "capital" Bool) where
  toParam _ =
    DocQueryParam "capital"
                  ["true", "false"]
                  "Get the greeting message in uppercase (true) or not (false).\
                  \Default is false."
                  Normal

-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON or PlainText
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON, PlainText] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] (Headers '[Header "X-Example" Int] Greet)

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent

testApi :: Proxy TestApi
testApi = Proxy

main :: IO ()
main = do
    _ <- parseHandleClient' testApi (Proxy @ClientM)
            (header "example" <> progDesc "Example API")
          $ (\(Greet s) -> T.unpack s)
       :<|> (show @(ResponseHeader "X-Example" Int) . lookupResponseHeader)
       :<|> const "deleted"
    pure ()
