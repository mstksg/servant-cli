{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.CLI (
    HasClient(..)
  , clientParser
  , parseClient
  ) where

import           Data.Bifunctor
import           Data.Proxy
import           GHC.TypeLits
import           Options.Applicative
import           Options.Applicative.Types
import           Servant.API
import           Servant.API.Modifiers
import           Servant.Client
import           Servant.Client.Core
import qualified Data.Text                 as T

class HasClient m api => HasCLI m api where
    type CLI m api

    clientParser_ :: Proxy m -> Proxy api -> Client m api -> Parser (m (CLI m api))

instance HasClient m EmptyAPI => HasCLI m EmptyAPI where
    type CLI m EmptyAPI = EmptyClient
    clientParser_ _ _ = pure . pure

instance (HasCLI m a, HasCLI m b) => HasCLI m (a :<|> b) where
    type CLI m (a :<|> b) = Either (CLI m a) (CLI m b)
    clientParser_ pm _ (cA :<|> cB) = (fmap Left  <$> clientParser_ pm (Proxy @a) cA)
                                 <|> (fmap Right <$> clientParser_ pm (Proxy @b) cB)

instance (KnownSymbol path, HasCLI m api) => HasCLI m (path :> api) where
    type CLI m (path :> api) = CLI m api
    clientParser_ pm _ api = subparser $
      command (symbolVal (Proxy @path))
              (info (clientParser_ pm (Proxy @api) api <**> helper) mempty)

instance ( KnownSymbol capture
         , FromHttpApiData a
         , ToHttpApiData a
         , HasCLI m api
         ) => HasCLI m (Capture' mods capture a :> api) where
    type CLI m (Capture' mods capture a :> api) = CLI m api
    clientParser_ pm _ api = BindP arg $ clientParser_ pm (Proxy @api) . api
      where
        arg :: Parser a
        arg = argument (eitherReader (first T.unpack . parseUrlPiece @a . T.pack))
                ( metavar (symbolVal (Proxy @capture)) )

instance ( KnownSymbol sym
         , FromHttpApiData a
         , ToHttpApiData a
         , SBoolI (FoldRequired' 'False mods)
         , HasCLI m api
         ) => HasCLI m (QueryParam' mods sym a :> api) where
    type CLI m (QueryParam' mods sym a :> api) = CLI m api
    clientParser_ pm _ api = BindP opt' $ clientParser_ pm (Proxy @api) . api
      where
        opt :: Parser a
        opt = option (eitherReader (first T.unpack . parseUrlPiece @a . T.pack))
                ( long (symbolVal (Proxy @sym)) )
        opt' :: Parser (If (FoldRequired' 'False mods) a (Maybe a))
        opt' = case sbool @(FoldRequired' 'False mods) of
          STrue  -> opt
          SFalse -> optional opt

instance ( KnownSymbol sym
         , HasCLI m api
         ) => HasCLI m (QueryFlag sym :> api) where
    type CLI m (QueryFlag sym :> api) = CLI m api
    clientParser_ pm _ api = BindP opt $ clientParser_ pm (Proxy @api) . api
      where
        opt :: Parser Bool
        opt = switch ( long (symbolVal (Proxy @sym)) )

instance ( cts' ~ (ct ': cts)
         , RunClient m
         , MimeUnrender ct a
         , ReflectMethod method
         ) => HasCLI m (Verb method status cts' a) where

    type CLI m (Verb method status cts' a) = a

    clientParser_ _ _ = pure

clientParser
    :: HasCLI m api
    => Proxy api
    -> Proxy m
    -> Parser (m (CLI m api))
clientParser papi pm = clientParser_ pm papi (clientIn papi pm)

parseClient
    :: HasCLI ClientM api
    => Proxy api
    -> IO (ClientM (CLI ClientM api))
parseClient p = execParser $ info (clientParser p (Proxy @ClientM) <**> helper) mempty

