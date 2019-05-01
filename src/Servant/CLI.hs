{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.CLI (
    HasClient(..)
  , clientPStruct
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
import           Data.Profunctor
import           Data.Proxy
import           GHC.TypeLits hiding   (Mod)
import           Options.Applicative
import           Servant.API
import           Servant.API.Modifiers
import           Servant.CLI.Structure
import           Servant.Client
import           Servant.Client.Core
import           Servant.Docs hiding   (Endpoint)
import           Text.Printf
import           Type.Reflection
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import qualified Network.HTTP.Types    as HTTP

_testStruct :: PStruct (Either (Bool, String) String)
_testStruct = note "test program" $ hello `branch` (greetDelete <> greetPost <> whatever)
  where
    hello       = "hello"
              $:> "the hello"
            `note` Arg "name" "name" "<name>" (str @String)
              #:> Opt "capital" "capital" "BOOL" orSwitch
              ?:> endpoint HTTP.methodGet (,)
    greetPost   = "greet"
              $:> defaultParseBody (str @String)
              %:> "post greet"
            `note` endpoint HTTP.methodPost (map toUpper)
    greetDelete = "greet"
              $:> Arg "greetid" "greetid" "<greetid>" (str @String)
              #:> endpoint HTTP.methodDelete (map toLower)
    whatever = "foo"
           $:> "bar"
           $:> "baz"
           $:> Arg "name" "name" "<name>" (str @String)
           #:> endpoint HTTP.methodGet reverse


class HasCLI m api where
    type CLI m api

    clientParser_
        :: Proxy m
        -> Proxy api
        -> PStruct (Request -> CLI m api)

-- | 'EmptyAPI' will always fail.
instance HasCLI m EmptyAPI where
    type CLI m EmptyAPI = EmptyClient
    clientParser_ _ _ = mempty

-- | Using alternation with ':<|>' provides an 'Either' between the two
-- results.
instance (HasCLI m a, HasCLI m b) => HasCLI m (a :<|> b) where
    type CLI m (a :<|> b) = Either (CLI m a) (CLI m b)
    clientParser_ pm _ = (fmap Left  <$> clientParser_ pm (Proxy @a))
                      <> (fmap Right <$> clientParser_ pm (Proxy @b))

-- | A path component is interpreted as a "subcommand".
instance (KnownSymbol path, HasCLI m api) => HasCLI m (path :> api) where
    type CLI m (path :> api) = CLI m api
    clientParser_ pm _ = pathstr $:>
        (fmap . lmap) (appendToPath (T.pack pathstr))
                      (clientParser_ pm (Proxy @api))
      where
        pathstr = symbolVal (Proxy @path)

-- | A 'Capture' is interpreted as a positional required command line argument.
instance ( FromHttpApiData a
         , ToHttpApiData a
         , Typeable a
         , ToCapture (Capture sym a)
         , HasCLI m api
         ) => HasCLI m (Capture' mods sym a :> api) where
    type CLI m (Capture' mods sym a :> api) = CLI m api
    clientParser_ pm _ = arg #:>
        (fmap . flip) (lmap . addCapture) (clientParser_ pm (Proxy @api))
      where
        addCapture = appendToPath . toUrlPiece
        arg = Arg
          { argName = _capSymbol
          , argDesc = printf "%s (%s)" _capDesc capType
          , argMeta = printf "<%s>" _capSymbol
          , argRead = eitherReader $ first T.unpack . parseUrlPiece @a . T.pack
          }
        capType = show $ typeRep @a
        DocCapture{..} = toCapture (Proxy @(Capture sym a))

-- | Query parameters are interpreted as command line options
instance ( KnownSymbol sym
         , FromHttpApiData a
         , ToHttpApiData a
         , SBoolI (FoldRequired' 'False mods)
         , Typeable a
         , ToParam (QueryParam' mods sym a)
         , HasCLI m api
         ) => HasCLI m (QueryParam' mods sym a :> api) where
    type CLI m (QueryParam' mods sym a :> api) = CLI m api
    clientParser_ pm _ = opt ?:>
        (fmap . flip) (lmap . addParam) (clientParser_ pm (Proxy @api))
      where
        addParam :: RequiredArgument mods a -> Request -> Request
        addParam = foldRequiredArgument (Proxy @mods) add (maybe id add)
        add :: a -> Request -> Request
        add param = appendToQueryString (T.pack pName) (Just (toQueryParam param))
        opt :: Opt (RequiredArgument mods a)
        opt = Opt
          { optName = pName
          , optDesc = printf "%s (%s)" _paramDesc pType
          , optMeta = map toUpper pType
          , optRead = case sbool @(FoldRequired mods) of
              STrue  -> orRequired r
              SFalse -> orOptional r
          }
        r     = eitherReader $ first T.unpack . parseQueryParam @a . T.pack
        pType = show $ typeRep @a
        pName = symbolVal (Proxy @sym)
        DocQueryParam{..} = toParam (Proxy @(QueryParam' mods sym a))
        -- TODO: experiment with more detailed help doc
        -- also, we can offer completion with values

-- | Query flags are interpreted as command line flags/switches
instance ( KnownSymbol sym
         , ToParam (QueryFlag sym)
         , HasCLI m api
         ) => HasCLI m (QueryFlag sym :> api) where
    type CLI m (QueryFlag sym :> api) = CLI m api
    clientParser_ pm _ = opt ?:>
        (fmap . flip) (lmap . addParam) (clientParser_ pm (Proxy @api))
      where
        addParam :: Bool -> Request -> Request
        addParam = \case
          True  -> appendToQueryString (T.pack pName) Nothing
          False -> id
        opt = Opt
          { optName = pName
          , optDesc = _paramDesc
          , optMeta = printf "<%s>" pName
          , optRead = orSwitch
          }
        pName = symbolVal (Proxy @sym)
        DocQueryParam{..} = toParam (Proxy @(QueryFlag sym))

class ParseBody a where
    parseBody :: Parser a

    default parseBody :: (Typeable a, Read a) => Parser a
    parseBody = defaultParseBody auto

-- | Request body requirements are interpreted using 'ParseBody'.  This
-- allows for more complicated parsing systems.
--
-- Note that both parsers will be "run" if more than one 'ReqBody' is in
-- an API endpoint, but only the final one will be used.
instance ( MimeRender ct a
         , ParseBody a
         , HasCLI m api
         ) => HasCLI m (ReqBody' mods (ct ': cts) a :> api) where
    type CLI m (ReqBody' mods (ct ': cts) a :> api) = CLI m api
    clientParser_ pm _ = parseBody @a %:>
        (fmap . flip) (lmap . addBody) (clientParser_ pm (Proxy @api))
      where
        ctProxy = Proxy @ct
        addBody b = setRequestBodyLBS (mimeRender ctProxy b) (contentType ctProxy)
    -- TODO: use tosample to provide samples?

instance ( HasClient m (Verb method status cts' a)
         , ReflectMethod method
         ) => HasCLI m (Verb method status cts' a) where
    type CLI m (Verb method status cts' a) = Client m (Verb method status cts' a)
    clientParser_ pm pa = endpoint (reflectMethod (Proxy @method)) (clientWithRoute pm pa)

clientPStruct
    :: HasCLI m api
    => Proxy api
    -> Proxy m
    -> PStruct (CLI m api)
clientPStruct pa pm = ($ defaultRequest) <$> clientParser_ pm pa

parseClient
    :: HasCLI ClientM api
    => Proxy api
    -> IO (CLI ClientM api)
parseClient p = execParser . structParser $ clientPStruct p (Proxy @ClientM)

-- TODO: use some meta var type instead of Typeable
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
