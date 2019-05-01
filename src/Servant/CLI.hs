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
    HasCLI(..)
  , clientPStruct
  , parseClient
  , ParseBody(..)
  , defaultParseBody
  , _testStruct
  , structParser
  -- * Re-export
  , ToSample(..)
  , ToCapture(..), DocCapture(..)
  , ToParam(..), DocQueryParam(..)
  ) where

import           Data.Bifunctor
import           Data.Char
import           Data.List
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
import qualified Data.List.NonEmpty    as NE
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import qualified Network.HTTP.Types    as HTTP

_testStruct :: PStruct (Either (Bool, String) String)
_testStruct = note "test program" $ hello `branch` (greetDelete <> greetPost <> whatever)
  where
    hello       = "hello"
              $:> "the hello"
           `note` Arg "name" "name" "<name>" (str @String)
              #:> Opt "capital" "capital" "BOOL" (NE.nonEmpty ["alice","bob","carol"]) orSwitch
              ?:> endpoint HTTP.methodGet (,)
    greetPost   = "greet"
              $:> parseBody @String
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


-- | Typeclass defining how each API combinator influences how a server can
-- be interacted with using command line options.
--
-- Unless you are adding new combinators to be used with APIs, you can
-- ignore this class.
class HasCLI m api where
    type CLI m api

    clientParser_
        :: Proxy m
        -> Proxy api
        -> PStruct (Request -> CLI m api)

-- | 'EmptyAPI' will always fail to parse.
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
          , optVals = NE.nonEmpty _paramValues
          , optRead = orSwitch
          }
        pName = symbolVal (Proxy @sym)
        DocQueryParam{..} = toParam (Proxy @(QueryFlag sym))

-- | A helper class for defining directly how to parse request bodies.
-- This allows more complex parsing of bodies.
class ParseBody a where
    parseBody :: Parser a

    default parseBody :: (Typeable a, Read a) => Parser a
    parseBody = defaultParseBody (show (typeRep @a)) auto

-- | Request body requirements are interpreted using 'ParseBody'.
--
-- Note if more than one 'ReqBody' is in an API endpoint, both parsers will
-- be "run", but only the final one will be used.
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
    clientParser_ pm pa = endpoint (reflectMethod (Proxy @method)) (clientWithRoute pm pa)

-- | Create a structure for a command line parser.
clientPStruct
    :: HasCLI m api
    => Proxy api
    -> Proxy m
    -> PStruct (CLI m api)
clientPStruct pa pm = ($ defaultRequest) <$> clientParser_ pm pa

-- | Parse a servant client; the result can be run.  A good choice of @m@
-- is 'ClientM'.
--
-- It takes options on how the top-level prompt is displayed when given
-- @"--help"@; it can be useful for adding a header or program description.
-- Otherwise, just use 'mempty'.
parseClient
    :: HasCLI m api
    => Proxy api
    -> Proxy m
    -> InfoMod (CLI m api)
    -> IO (CLI m api)
parseClient pa pm im = execParser . flip structParser im $ clientPStruct pa pm

-- TODO: use some meta var type instead of Typeable
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
