{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.CLI (
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Functor.Foldable
import           Data.Kind
import           Data.Map                  (Map)
import           Data.Proxy
import           Data.Text                 (Text)
import           GHC.TypeLits
import           Options.Applicative
import           Options.Applicative.Types
import           Servant.API
import           Servant.API.Modifiers
import           Servant.Client
import           Servant.Client.Core
import qualified Data.Text                 as T

-- data Endpoint = Parser

-- data Opts = Branch (Parser (ClientM String))
--           | Command (Map String Opts)

-- data Opts = Opts (String -> Maybe Opts)
--           | DoIt (ClientM String)


-- class HasCLI api where
--     type CLI api
--     clientParser :: proxy api -> Parser (ClientM (CLI api))

-- instance HasCLI EmptyAPI where
--     type CLI EmptyAPI = EmptyClient
--     clientParser _ = pure . pure $ EmptyClient

-- instance (HasCLI a, HasCLI b) => HasCLI (a :<|> b) where
--     type CLI (a :<|> b) = Either (CLI a) (CLI b)
--     clientParser _ = (fmap Left  <$> clientParser (Proxy @a))
--                  <|> (fmap Right <$> clientParser (Proxy @b))

-- instance (KnownSymbol path, HasCLI api) => HasCLI (path :> api) where
--     type CLI (path :> api) = CLI api
--     clientParser _ = subparser $
--       command (symbolVal (Proxy @path))
--               (info (clientParser (Proxy @api) <**> helper) mempty)

-- instance (KnownSymbol capture, FromHttpApiData a, ToHttpApiData a, HasCLI api, HasClient ClientM api) => HasCLI (Capture' mods capture a :> api) where
--     type CLI (Capture' mods capture a :> api) = CLI api
--     clientParser _ = _ . client (Proxy @(Capture' mods capture a :> api)) <$> opt
--                                    -- (clientParser (Proxy @api))
--       where
--         opt = argument (eitherReader (first T.unpack . parseUrlPiece @a . T.pack))
--                 ( metavar "CAPTURE"
--                <> help (symbolVal (Proxy @capture))
--                 )

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

    clientParser_ pm _ = pure

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

