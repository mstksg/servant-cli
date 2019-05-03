{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import           Data.Proxy
import           Data.Text                (Text)
import           Data.Vinyl
import           GHC.Generics
import           Network.HTTP.Client      (newManager, defaultManagerSettings)
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (header, progDesc)
import           Servant.API
import           Servant.CLI
import           Servant.Client
import           Servant.Server
import           System.Random
import qualified Data.ByteString          as BS
import qualified Data.Map                 as M
import qualified Data.Text                as T


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

-- We add some useful annotations to our captures,
-- query parameters and request body to make the docs
-- really helpful.
instance ToCapture (Capture "name" Text) where
    toCapture _ = DocCapture "name" "name of the person to greet"

instance ToParam (QueryParam "capital" Bool) where
    toParam _ =
      DocQueryParam "capital"
                    ["true", "false"]
                    "Get the greeting message in uppercase (true) or not (false). Default is false."
                    Normal

instance ToAuthInfo (BasicAuth "login" Int) where
    toAuthInfo _ =
      DocAuthentication "Login credientials"
                        "Username and password"

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


testApi :: Proxy TestApi
testApi = Proxy

server :: Application
server = serveWithContext testApi (authCheck :. EmptyContext) $
        (\t b -> pure . Greet $ "Hello, "
              <> if fromMaybe False b
                    then T.toUpper t
                    else t
        )
   :<|> (\(Greet g) -> pure (T.length g)
                  :<|> (\_ -> pure NoContent)
        )
   :<|> (pure . T.reverse)
  where
    -- | Map of valid users and passwords
    userMap = M.fromList [("alice", "password"), ("bob", "hunter2")]
    authCheck = BasicAuthCheck $ \(BasicAuthData u p) ->
      case M.lookup u userMap of
        Nothing -> pure NoSuchUser
        Just p'
          | p == p'   -> Authorized <$> randomIO @Int
          | otherwise -> pure BadPassword

main :: IO ()
main = do
    c <- parseHandleClientWithContext
                    testApi
                    (Proxy :: Proxy ClientM)
                    (getPwd :& RNil)
                    cinfo $
            (\(Greet g) -> "Greeting: " ++ T.unpack g)
       :<|> ( (\i -> show i ++ " letters")
         :<|> (\_ -> "posted!")
            )
       :<|> (\s -> "Reversed: " ++ T.unpack s)

    _ <- forkIO $ run 8081 server

    manager' <- newManager defaultManagerSettings
    res      <- runClientM c (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))

    case res of
      Left e        -> throwIO e
      Right rstring -> putStrLn rstring
  where
    cinfo = header "greet" <> progDesc "Greet API"
    getPwd :: ContextFor ClientM (BasicAuth "login" Int)
    getPwd = GenBasicAuthData . liftIO $ do
      putStrLn "Authentication needed for this action!"
      putStrLn "(Hint: try 'bob' and 'hunter2')"
      putStrLn "Enter username:"
      n <- BS.getLine
      putStrLn "Enter password:"
      p <- BS.getLine
      pure $ BasicAuthData n p
