{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.CLI (
    HasClient(..)
  , clientParser
  , parseClient
  , ParseBody(..)
  , defaultParseBody
  -- * Re-export
  , ToSample(..)
  , ToCapture(..), DocCapture(..)
  , ToParam(..), DocQueryParam(..)
  ) where

import           Data.Bifunctor
import           Data.Char
import           Data.Proxy
import           GHC.TypeLits
import           Options.Applicative
import           Options.Applicative.Types
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.API.Modifiers
import           Servant.Client
import           Servant.Client.Core
import           Servant.Docs
import           Servant.Docs.Internal
import           Text.Printf
import           Type.Reflection
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL

class (HasDocs api, HasClient m api) => HasCLI m api where
    type CLI m api

    clientParser_
        :: Proxy m
        -> Proxy api
        -> Client m api
        -> Parser (m (CLI m api))

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
         command pathstr ( info (clientParser_ pm (Proxy @api) api <**> helper) mempty)
      <> metavar pathstr
      where
        pathstr = symbolVal (Proxy @path)

instance ( KnownSymbol sym
         , FromHttpApiData a
         , ToHttpApiData a
         , Typeable a
         , ToCapture (Capture sym a)
         , HasCLI m api
         ) => HasCLI m (Capture' mods sym a :> api) where
    type CLI m (Capture' mods sym a :> api) = CLI m api
    clientParser_ pm _ api = BindP arg $ clientParser_ pm (Proxy @api) . api
      where
        arg :: Parser a
        arg = argument (eitherReader (first T.unpack . parseUrlPiece @a . T.pack))
                ( metavar (map toUpper _capSymbol)
               <> help (printf "%s (%s)" _capDesc capType)
                )
        capType = show $ typeRep @a
        DocCapture{..} = toCapture (Proxy @(Capture sym a))

instance ( KnownSymbol sym
         , FromHttpApiData a
         , ToHttpApiData a
         , SBoolI (FoldRequired' 'False mods)
         , Typeable a
         , ToParam (QueryParam' mods sym a)
         , HasCLI m api
         ) => HasCLI m (QueryParam' mods sym a :> api) where
    type CLI m (QueryParam' mods sym a :> api) = CLI m api
    clientParser_ pm _ api = BindP opt' $ clientParser_ pm (Proxy @api) . api
      where
        opt :: Parser a
        opt = option (eitherReader (first T.unpack . parseUrlPiece @a . T.pack))
                ( metavar (map toUpper pType)
               <> long pName
               <> help (printf "%s (%s)" _paramDesc pType)
                )
        opt' :: Parser (If (FoldRequired' 'False mods) a (Maybe a))
        opt' = case sbool @(FoldRequired' 'False mods) of
          STrue  -> opt
          SFalse -> optional opt
        pType = show $ typeRep @a
        pName = symbolVal (Proxy @sym)
        DocQueryParam{..} = toParam (Proxy @(QueryParam' mods sym a))
        -- TODO: experiment with more detailed help doc
        -- also, we can offer completion with values

instance ( KnownSymbol sym
         , ToParam (QueryFlag sym)
         , HasCLI m api
         ) => HasCLI m (QueryFlag sym :> api) where
    type CLI m (QueryFlag sym :> api) = CLI m api
    clientParser_ pm _ api = BindP opt $ clientParser_ pm (Proxy @api) . api
      where
        opt :: Parser Bool
        opt = switch ( long (symbolVal (Proxy @sym)) )

class ParseBody a where
    parseBody :: Parser a

    default parseBody :: (Typeable a, Read a) => Parser a
    parseBody = defaultParseBody auto

instance ( MimeRender ct a
         , ParseBody a
         , ToSample a
         , AllMimeRender (ct ': cts) a
         , HasCLI m api
         ) => HasCLI m (ReqBody' mods (ct ': cts) a :> api) where
    type CLI m (ReqBody' mods (ct ': cts) a :> api) = CLI m api
    clientParser_ pm _ api =
        BindP parseBody $
          clientParser_ pm (Proxy @api) . api
    -- TODO: use tosample to provide samples?

-- TODO: we have a problem here, how to distinguish DELETE and POST
-- requests with the same name.
instance {-# OVERLAPPABLE #-}
    -- Note [Non-Empty Content Types]
    ( RunClient m, MimeUnrender ct a, ReflectMethod method, cts' ~ (ct ': cts)
    , ToSample a
    , AllMimeRender (ct ': cts) a
    , KnownNat status
    ) => HasCLI m (Verb method status cts' a) where
    type CLI m (Verb method status cts' a) = a
    clientParser_ _ _ = pure

instance {-# OVERLAPPING #-}
    ( RunClient m, ReflectMethod method
    , HasDocs (Verb method status cts NoContent)
    ) => HasCLI m (Verb method status cts NoContent) where
    type CLI m (Verb method status cts NoContent) = NoContent
    clientParser_ _ _ = pure

instance {-# OVERLAPPING #-}
    -- Note [Non-Empty Content Types]
    ( RunClient m, MimeUnrender ct a, BuildHeadersTo ls
    , ReflectMethod method, cts' ~ (ct ': cts)
    , ToSample a
    , AllMimeRender (ct ': cts) a
    , KnownNat status
    , AllHeaderSamples ls
    , GetHeaders (HList ls)
    ) => HasCLI m (Verb method status cts' (Headers ls a)) where
    type CLI m (Verb method status cts' (Headers ls a)) = Headers ls a
    clientParser_ _ _ = pure

instance {-# OVERLAPPING #-}
    ( RunClient m, BuildHeadersTo ls, ReflectMethod method
    , HasDocs (Verb method status cts (Headers ls NoContent))
    ) => HasCLI m (Verb method status cts (Headers ls NoContent)) where
    type CLI m (Verb method status cts (Headers ls NoContent))
      = Headers ls NoContent
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




defaultParseBody :: forall a. Typeable a => ReadM a -> Parser a
defaultParseBody r = option r
    ( metavar (map toUpper tp)
   <> long "data"
   <> short 'd'
   <> help (printf "Request body (%s)" tp)
    )
  where
    tp = show (typeRep @a)

instance ParseBody String where
    parseBody = defaultParseBody str

instance ParseBody T.Text where
    parseBody = defaultParseBody str

instance ParseBody TL.Text where
    parseBody = defaultParseBody str

instance ParseBody Int where
instance ParseBody Integer where
instance ParseBody Float where
instance ParseBody Double where
