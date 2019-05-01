{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.CLI.Internal (
    HasCLI(..)
  , ParseBody(..)
  , defaultParseBody
  ) where

import           Data.Bifunctor
import           Data.Char
import           Data.Kind
import           Data.List
import           Data.Profunctor
import           Data.Proxy
import           GHC.TypeLits hiding   (Mod)
import           Options.Applicative
import           Servant.API hiding    (addHeader)
import           Servant.API.Modifiers
import           Servant.CLI.Structure
import           Servant.Client
import           Servant.Client.Core
import           Servant.Docs hiding   (Endpoint)
import           Text.Printf
import           Type.Reflection
import qualified Data.CaseInsensitive  as CI
import qualified Data.List.NonEmpty    as NE
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Text.Lazy        as TL

-- | Typeclass defining how each API combinator influences how a server can
-- be interacted with using command line options.
--
-- Unless you are adding new combinators to be used with APIs, you can
-- ignore this class.
class HasCLI m api where
    type CLI (m :: Type -> Type) (api :: Type) :: Type

    clientPStruct_
        :: Proxy m
        -> Proxy api
        -> PStruct (Request -> CLI m api)

-- | 'EmptyAPI' will always fail to parse.
instance HasCLI m EmptyAPI where
    type CLI m EmptyAPI = EmptyClient
    clientPStruct_ _ _ = mempty

-- | Using alternation with ':<|>' provides an 'Either' between the two
-- results.
instance (HasCLI m a, HasCLI m b) => HasCLI m (a :<|> b) where
    type CLI m (a :<|> b) = Either (CLI m a) (CLI m b)
    clientPStruct_ pm _ = (fmap Left  <$> clientPStruct_ pm (Proxy @a))
                      <> (fmap Right <$> clientPStruct_ pm (Proxy @b))

-- | A path component is interpreted as a "subcommand".
instance (KnownSymbol path, HasCLI m api) => HasCLI m (path :> api) where
    type CLI m (path :> api) = CLI m api
    clientPStruct_ pm _ = pathstr $:>
        (fmap . lmap) (appendToPath (T.pack pathstr))
                      (clientPStruct_ pm (Proxy @api))
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
    clientPStruct_ pm _ = arg #:>
        (fmap . flip) (lmap . addCapture) (clientPStruct_ pm (Proxy @api))
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

-- | A 'CaptureAll' is interpreted as arbitrarily many command line
-- arguments.  If there is more than one final endpoint method, the method
-- must be given as a command line option before beginning the arguments.
instance ( FromHttpApiData a
         , ToHttpApiData a
         , Typeable a
         , ToCapture (CaptureAll sym a)
         , HasCLI m api
         ) => HasCLI m (CaptureAll sym a :> api) where
    type CLI m (CaptureAll sym a :> api) = CLI m api
    clientPStruct_ pm _ = arg ##:>
        (fmap . flip) (lmap . addCapture) (clientPStruct_ pm (Proxy @api))
      where
        addCapture ps req = foldl' (flip appendToPath) req (map toUrlPiece ps)
        arg = Arg
          { argName = _capSymbol
          , argDesc = printf "%s (%s)" _capDesc capType
          , argMeta = printf "<%s>" _capSymbol
          , argRead = eitherReader $ first T.unpack . parseUrlPiece @a . T.pack
          }
        capType = show $ typeRep @a
        DocCapture{..} = toCapture (Proxy @(CaptureAll sym a))

-- | Query parameters are interpreted as command line options.
--
-- 'QueryParam'' arguments are associated with the action at their
-- endpoint.  After entering all path components and positional arguments,
-- the parser library will begin asking for arguments.
instance ( KnownSymbol sym
         , FromHttpApiData a
         , ToHttpApiData a
         , SBoolI (FoldRequired' 'False mods)
         , Typeable a
         , ToParam (QueryParam' mods sym a)
         , HasCLI m api
         ) => HasCLI m (QueryParam' mods sym a :> api) where
    type CLI m (QueryParam' mods sym a :> api) = CLI m api
    clientPStruct_ pm _ = opt ?:>
        (fmap . flip) (lmap . addParam) (clientPStruct_ pm (Proxy @api))
      where
        addParam :: RequiredArgument mods a -> Request -> Request
        addParam = foldRequiredArgument (Proxy @mods) add (maybe id add)
        add :: a -> Request -> Request
        add param = appendToQueryString (T.pack pName) (Just (toQueryParam param))
        opt :: Opt (RequiredArgument mods a)
        opt = Opt
          { optName = pName
          , optDesc = printf "%s (%s)" _paramDesc valSpec
          , optMeta = map toUpper pType
          , optVals = NE.nonEmpty _paramValues
          , optRead = case sbool @(FoldRequired mods) of
              STrue  -> orRequired r
              SFalse -> orOptional r
          }
        r     = eitherReader $ first T.unpack . parseQueryParam @a . T.pack
        pType = show $ typeRep @a
        valSpec
          | null _paramValues = pType
          | otherwise         = "options: " ++ intercalate ", " _paramValues
        pName = symbolVal (Proxy @sym)
        DocQueryParam{..} = toParam (Proxy @(QueryParam' mods sym a))
        -- TODO: experiment with more detailed help doc

-- | Query flags are interpreted as command line flags/switches.
--
-- 'QueryFlag' arguments are associated with the action at their endpoint.
-- After entering all path components and positional arguments, the parser
-- library will begin asking for arguments.
instance ( KnownSymbol sym
         , ToParam (QueryFlag sym)
         , HasCLI m api
         ) => HasCLI m (QueryFlag sym :> api) where
    type CLI m (QueryFlag sym :> api) = CLI m api
    clientPStruct_ pm _ = opt ?:>
        (fmap . flip) (lmap . addParam) (clientPStruct_ pm (Proxy @api))
      where
        addParam :: Bool -> Request -> Request
        addParam = \case
          True  -> appendToQueryString (T.pack pName) Nothing
          False -> id
        opt = Opt
          { optName = pName
          , optDesc = _paramDesc
          , optMeta = printf "<%s>" pName
          , optVals = NE.nonEmpty _paramValues
          , optRead = orSwitch
          }
        pName = symbolVal (Proxy @sym)
        DocQueryParam{..} = toParam (Proxy @(QueryFlag sym))

-- | Request body requirements are interpreted using 'ParseBody'.
--
-- Note if more than one 'ReqBody' is in an API endpoint, both parsers will
-- be "run", but only the final one will be used.
instance ( MimeRender ct a
         , ParseBody a
         , HasCLI m api
         ) => HasCLI m (ReqBody' mods (ct ': cts) a :> api) where
    type CLI m (ReqBody' mods (ct ': cts) a :> api) = CLI m api
    clientPStruct_ pm _ = parseBody @a %:>
        (fmap . flip) (lmap . addBody) (clientPStruct_ pm (Proxy @api))
      where
        ctProxy = Proxy @ct
        addBody b = setRequestBodyLBS (mimeRender ctProxy b) (contentType ctProxy)
    -- TODO: use tosample to provide samples?

-- | Final actions are the result of specifying all necessary command line
-- positional arguments.
--
-- All command line options are associated with the final action at the end
-- of their endpoint/path.  They cannot be entered in "before" you arrive
-- at your final endpoint.
--
-- If more than one action (under a different method) exists
-- under the same endpoint/path, the method (@GET@, @POST@, etc.) will be
-- treated as an extra final command.  After that, you may begin entering
-- in options.
instance ( HasClient m (Verb method status cts' a)
         , ReflectMethod method
         ) => HasCLI m (Verb method status cts' a) where
    type CLI m (Verb method status cts' a) = Client m (Verb method status cts' a)
    clientPStruct_ pm pa = endpoint (reflectMethod (Proxy @method)) (clientWithRoute pm pa)

-- instance {-# OVERLAPPABLE #-}
--   ( RunStreamingClient m, MimeUnrender ct chunk, ReflectMethod method,
--     FramingUnrender framing, FromSourceIO chunk a
--   ) => HasClient m (Stream method status framing ct a) where

-- instance
--     ( HasClient m api, MimeRender ctype chunk, FramingRender framing, ToSourceIO chunk a
--     ) => HasClient m (StreamBody' mods framing ctype a :> api)

-- | A 'Header' in the middle of a path is interpreted as a command line
-- argument, prefixed with "header".  For example, @'Header' "foo" 'Int'@
-- is an option for @--header-foo@.
--
-- Like for 'QueryParam'',  arguments are associated with the action at
-- their endpoint.  After entering all path components and positional
-- arguments, the parser library will begin asking for arguments.
instance ( KnownSymbol sym
         , FromHttpApiData a
         , ToHttpApiData a
         , SBoolI (FoldRequired' 'False mods)
         , Typeable a
         , HasCLI m api
         ) => HasCLI m (Header' mods sym a :> api) where
    type CLI m (Header' mods sym a :> api) = CLI m api
    clientPStruct_ pm _ = opt ?:>
        (fmap . flip) (lmap . addParam) (clientPStruct_ pm (Proxy @api))
      where
        addParam :: RequiredArgument mods a -> Request -> Request
        addParam = foldRequiredArgument (Proxy @mods) add (maybe id add)
        add :: a -> Request -> Request
        add v = addHeader (CI.mk . T.encodeUtf8 . T.pack $ pName) v
        opt :: Opt (RequiredArgument mods a)
        opt = Opt
          { optName = printf "header-%s" pName
          , optDesc = printf "Header data %s (%s)" pName pType
          , optMeta = map toUpper pType
          , optVals = Nothing
          , optRead = case sbool @(FoldRequired mods) of
              STrue  -> orRequired r
              SFalse -> orOptional r
          }
        r :: ReadM a
        r     = eitherReader $ first T.unpack . parseHeader . T.encodeUtf8 . T.pack
        pType = show $ typeRep @a
        pName = symbolVal (Proxy @sym)

-- | Using 'HttpVersion' has no affect on CLI operations.
instance HasCLI m api => HasCLI m (HttpVersion :> api) where
    type CLI m (HttpVersion :> api) = CLI m api
    clientPStruct_ pm _ = clientPStruct_ pm (Proxy @api)

-- | 'Summary' is displayed during @--help@ when it is reached while
-- navigating down subcommands.
instance (KnownSymbol desc, HasCLI m api) => HasCLI m (Summary desc :> api) where
  type CLI m (Summary desc :> api) = CLI m api

  clientPStruct_ pm _ = note (symbolVal (Proxy @desc))
                      $ clientPStruct_ pm (Proxy :: Proxy api)

-- | 'Description' is displayed during @--help@ when it is reached while
-- navigating down subcommands.
instance (KnownSymbol desc, HasCLI m api) => HasCLI m (Description desc :> api) where
  type CLI m (Description desc :> api) = CLI m api

  clientPStruct_ pm _ = note (symbolVal (Proxy @desc))
                      $ clientPStruct_ pm (Proxy :: Proxy api)


-- instance HasCLI m Raw where

instance HasCLI m api => HasCLI m (Vault :> api) where
    type CLI m (Vault :> api) = CLI m api

    clientPStruct_ pm _ = clientPStruct_ pm (Proxy @api)

instance HasCLI m api => HasCLI m (RemoteHost :> api) where
    type CLI m (RemoteHost :> api) = CLI m api

    clientPStruct_ pm _ = clientPStruct_ pm (Proxy @api)

instance HasCLI m api => HasCLI m (IsSecure :> api) where
    type CLI m (IsSecure :> api) = CLI m api

    clientPStruct_ pm _ = clientPStruct_ pm (Proxy @api)

instance HasCLI m subapi => HasCLI m (WithNamedContext name context subapi) where
    type CLI m (WithNamedContext name context subapi) = CLI m subapi

    clientPStruct_ pm _ = clientPStruct_ pm (Proxy @subapi)

-- | Adding 'AuthProtect' means that the result of parsing command line
-- options will be a function that takes a 'AuthenticatedRequest' and
-- returns an @m@ action to make a request to the server with that
-- authentication data.
--
-- Please use a secure connection!
instance HasCLI m api => HasCLI m (AuthProtect tag :> api) where
    type CLI m (AuthProtect tag :> api) = AuthenticatedRequest (AuthProtect tag) -> CLI m api

    clientPStruct_ pm _ = addAuth <$> clientPStruct_ pm (Proxy @api)
      where
        addAuth f r (AuthenticatedRequest (val, func)) = f . func val $ r

-- | Adding 'BasicAuth' means that the result of parsing command line
-- options will be a function that takes a 'BasicAuthData' and returns an
-- @m@ action to make a request to the server with that authentication
-- data.
--
-- Please use a secure connection!
instance HasCLI m api => HasCLI m (BasicAuth realm usr :> api) where
    type CLI m (BasicAuth realm usr :> api) = BasicAuthData -> CLI m api

    clientPStruct_ pm _ = addAuth <$> clientPStruct_ pm (Proxy @api)
      where
        addAuth
            :: (Request -> CLI m api)
            -> Request
            -> BasicAuthData
            -> CLI m api
        addAuth f r d = f . basicAuthReq d $ r


-- | A helper class for defining directly how to parse request bodies.
-- This allows more complex parsing of bodies.
class ParseBody a where
    parseBody :: Parser a

    default parseBody :: (Typeable a, Read a) => Parser a
    parseBody = defaultParseBody (show (typeRep @a)) auto

defaultParseBody
    :: String       -- ^ type specification
    -> ReadM a      -- ^ parser
    -> Parser a
defaultParseBody mv r = option r
    ( metavar (printf "<%s>" (map toLower mv))
   <> long "data"
   <> short 'd'
   <> help (printf "Request body (%s)" mv)
    )

instance ParseBody String where
    parseBody = defaultParseBody "Text" str

instance ParseBody T.Text where
    parseBody = defaultParseBody "Text" str

instance ParseBody TL.Text where
    parseBody = defaultParseBody "Text" str

instance ParseBody Int where
instance ParseBody Integer where
instance ParseBody Float where
instance ParseBody Double where
