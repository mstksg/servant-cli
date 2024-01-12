{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- |
-- Module      : Servant.CLI.HasCLI
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Main module providing underlying functionality for the command line
-- interface parser for servant API clients.
--
-- For the most part, you can ignore this module unless you're adding new
-- API combinators.
module Servant.CLI.HasCLI (
  -- * Class
    HasCLI(..)
  -- * Context
  , ContextFor(..)
  , NamedContext(..)
  , descendIntoNamedContext
  ) where

import           Data.Bifunctor
import           Data.Char
import           Data.Function
import           Data.Kind
import           Data.List
import           Data.Profunctor
import           Data.Proxy
import           Data.Vinyl hiding            (rmap)
import           Data.Void
import           GHC.TypeLits hiding          (Mod)
import           Options.Applicative
import           Servant.API hiding           (addHeader)
import           Servant.API.Modifiers
import           Servant.CLI.Internal.PStruct
import           Servant.CLI.ParseBody
import           Servant.Client.Core
import           Servant.Docs.Internal hiding (Endpoint, Response)
import           Text.Printf
import           Type.Reflection
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Builder      as BSB
import qualified Data.CaseInsensitive         as CI
import qualified Data.List.NonEmpty           as NE
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T

-- | Data family associating API combinators with contexts required to run
-- them.  These typically will be actions in @m@ that fetch/generate the
-- required data, and will only be "run" if the user selects an endpoint
-- that requires it through the command line interface.
data family ContextFor (m :: Type -> Type) :: Type -> Type


-- | Typeclass defining how each API combinator influences how a server can
-- be interacted with using command line options.
--
-- Note that query parameters and captures all require /servant-docs/
-- annotation instances, to allow for proper help messages.
--
-- Unless you are adding new combinators to be used with APIs, you can
-- ignore this class.
class HasCLI m api ctx where

    -- | The parsed type of the client request response.  Usually this will
    -- be a bunch of nested 'Either's for every API endpoint, nested
    -- according to the ':<|>'s in the API.
    type CLIResult  (m :: Type -> Type) (api :: Type) :: Type

    -- | The type of a data structure to conveniently handle the results of
    -- all pontential endpoints.  This is useful because it is often
    -- tedious to handle the bunch of nested 'Either's that 'CLIResult'
    -- has.
    --
    -- It essentially lets you specify how to sort each potential
    -- endpoint's response into a single output value.
    --
    -- Usually this will be a bunch of nested ':<|>'s which handle each
    -- endpoint, according to the ':<|>'s in the API.  It mirrors the
    -- structure of 'Client' and 'Servant.Server.ServerT'.
    --
    -- Used with functions like 'Servant.CLI.parseHandleClient'.
    type CLIHandler (m :: Type -> Type) (api :: Type) (r :: Type) :: Type

    -- | Create a structure for a command line parser, which parses how to
    -- modify a 'Request' and perform an action, given an API and
    -- underlying monad.  Only meant for internal use; should be used
    -- through 'Servant.CLI.cliPStructWithContext' instead.
    --
    -- Takes a 'Rec' of actions to generate required items that cannot be
    -- passed via the command line (like authentication).  Pass in 'RNil'
    -- if no parameters are expected.  The actions will only be run if they
    -- are needed.
    cliPStructWithContext_
        :: Proxy m
        -> Proxy api
        -> Rec (ContextFor m) ctx
        -> PStruct (Request -> m (CLIResult m api))

    -- | Handle all the possibilities in a 'CLIResult', by giving the
    -- appropriate 'CLIHandler'.
    cliHandler
        :: Proxy m
        -> Proxy api
        -> Proxy ctx
        -> CLIHandler m api r
        -> CLIResult m api
        -> r

-- | 'EmptyAPI' will always fail to parse.
--
-- The branch ending in 'EmptyAPI' will never be return, so if this is
-- combined using ':<|>', the branch will never end up on the side of
-- 'EmptyAPI'.
--
-- One can use 'absurd' to handle this branch as a part of 'CLIHandler'.
instance HasCLI m EmptyAPI ctx where
    type CLIResult  m EmptyAPI   = Void
    type CLIHandler m EmptyAPI r = Void -> r

    cliPStructWithContext_ _ _ _ = mempty
    cliHandler  _ _ _ = ($)

-- | Using alternation with ':<|>' provides an 'Either' between the two
-- results.
instance ( HasCLI m a ctx
         , HasCLI m b ctx
         , Functor m
         ) => HasCLI m (a :<|> b) ctx where
    type CLIResult  m (a :<|> b)   = Either (CLIResult m a) (CLIResult m b)
    type CLIHandler m (a :<|> b) r = CLIHandler m a r :<|> CLIHandler m b r

    cliPStructWithContext_ pm _ p =
          dig Left  (cliPStructWithContext_ pm (Proxy @a) p)
       <> dig Right (cliPStructWithContext_ pm (Proxy @b) p)
      where
        dig = fmap . rmap . fmap

    cliHandler pm _ pc (hA :<|> hB) = either (cliHandler pm (Proxy @a) pc hA)
                                             (cliHandler pm (Proxy @b) pc hB)

-- | A path component is interpreted as a "subcommand".
instance (KnownSymbol path, HasCLI m api ctx) => HasCLI m (path :> api) ctx where
    type CLIResult  m (path :> api)   = CLIResult m api
    type CLIHandler m (path :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ p = pathstr $:>
        (fmap . lmap)
          (appendToPath (BSB.byteString $ T.encodeUtf8 $ T.pack pathstr))
          (cliPStructWithContext_ pm (Proxy @api) p)
      where
        pathstr = symbolVal (Proxy @path)

    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | A 'Capture' is interpreted as a positional required command line argument.
--
-- Note that these require 'ToCapture' instances from /servant-docs/, to
-- provide appropriate help messages.
instance ( FromHttpApiData a
         , ToHttpApiData a
         , Typeable a
         , ToCapture (Capture sym a)
         , HasCLI m api ctx
         ) => HasCLI m (Capture' mods sym a :> api) ctx where
    type CLIResult  m (Capture' mods sym a :> api)   = CLIResult m api
    type CLIHandler m (Capture' mods sym a :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ p = arg #:>
        fmap (.: addCapture) (cliPStructWithContext_ pm (Proxy @api) p)
      where
        addCapture = appendToPath . BSB.byteString . T.encodeUtf8 . toUrlPiece
        arg = Arg
          { argName = _capSymbol
          , argDesc = printf "%s (%s)" _capDesc capType
          , argMeta = printf "<%s>" _capSymbol
          , argRead = eitherReader $ first T.unpack . parseUrlPiece @a . T.pack
          }
        capType = show $ typeRep @a
        DocCapture{..} = toCapture (Proxy @(Capture sym a))

    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | A 'CaptureAll' is interpreted as arbitrarily many command line
-- arguments.  If there is more than one final endpoint method, the method
-- must be given as a command line option before beginning the arguments.
instance ( FromHttpApiData a
         , ToHttpApiData a
         , Typeable a
         , ToCapture (CaptureAll sym a)
         , HasCLI m api ctx
         ) => HasCLI m (CaptureAll sym a :> api) ctx where
    type CLIResult  m (CaptureAll sym a :> api)   = CLIResult m api
    type CLIHandler m (CaptureAll sym a :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ p = arg ##:>
        fmap (.: addCapture) (cliPStructWithContext_ pm (Proxy @api) p)
      where
        addCapture ps req = foldl'
          (flip appendToPath)
          req
          (map (BSB.byteString . T.encodeUtf8 . toUrlPiece) ps)
        arg = Arg
          { argName = _capSymbol
          , argDesc = printf "%s (%s)" _capDesc capType
          , argMeta = printf "<%s>" _capSymbol
          , argRead = eitherReader $ first T.unpack . parseUrlPiece @a . T.pack
          }
        capType = show $ typeRep @a
        DocCapture{..} = toCapture (Proxy @(CaptureAll sym a))

    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | Query parameters are interpreted as command line options.
--
-- 'QueryParam'' arguments are associated with the action at their
-- endpoint.  After entering all path components and positional arguments,
-- the parser library will begin asking for arguments.
--
-- Note that these require 'ToParam' instances from /servant-docs/, to
-- provide appropriate help messages.
instance ( KnownSymbol sym
         , FromHttpApiData a
         , ToHttpApiData a
         , SBoolI (FoldRequired' 'False mods)
         , Typeable a
         , ToParam (QueryParam' mods sym a)
         , HasCLI m api ctx
         ) => HasCLI m (QueryParam' mods sym a :> api) ctx where
    type CLIResult  m (QueryParam' mods sym a :> api)   = CLIResult m api
    type CLIHandler m (QueryParam' mods sym a :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ p = opt ?:>
        fmap (.: addParam) (cliPStructWithContext_ pm (Proxy @api) p)
      where
        addParam :: RequiredArgument mods a -> Request -> Request
        addParam = foldRequiredArgument (Proxy @mods) add (maybe id add)
        add :: a -> Request -> Request
        add param = appendToQueryString
              (T.pack pName)
              (Just (T.encodeUtf8 $ toQueryParam param))
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

    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | Query flags are interpreted as command line flags/switches.
--
-- 'QueryFlag' arguments are associated with the action at their endpoint.
-- After entering all path components and positional arguments, the parser
-- library will begin asking for arguments.
--
-- Note that these require 'ToParam' instances from /servant-docs/, to
-- provide appropriate help messages.
instance ( KnownSymbol sym
         , ToParam (QueryFlag sym)
         , HasCLI m api ctx
         ) => HasCLI m (QueryFlag sym :> api) ctx where
    type CLIResult  m (QueryFlag sym :> api)   = CLIResult m api
    type CLIHandler m (QueryFlag sym :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ p = opt ?:>
        fmap (.: addParam) (cliPStructWithContext_ pm (Proxy @api) p)
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

    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | Request body requirements are interpreted using 'ParseBody'.
--
-- Note if more than one 'ReqBody' is in an API endpoint, both parsers will
-- be "run", but only the final one will be used.  This shouldn't be an
-- issue, since multiple 'ReqBody's in a single endpoint should be
-- undefined behavior.
instance ( MimeRender ct a
         , ParseBody a
         , HasCLI m api ctx
         ) => HasCLI m (ReqBody' mods (ct ': cts) a :> api) ctx where
    type CLIResult  m (ReqBody' mods (ct ': cts) a :> api)   = CLIResult m api
    type CLIHandler m (ReqBody' mods (ct ': cts) a :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ p = parseBody @a %:>
        fmap (.: addBody) (cliPStructWithContext_ pm (Proxy @api) p)
      where
        addBody b = setRequestBodyLBS (mimeRender ctProxy b) (contentType ctProxy)
        ctProxy = Proxy @ct

    cliHandler pm _ = cliHandler pm (Proxy @api)

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
         ) => HasCLI m (Verb method status cts' a) ctx where
    type CLIResult  m (Verb method status cts' a)   = a
    type CLIHandler m (Verb method status cts' a) r = a -> r

    cliPStructWithContext_ pm pa _ = endpoint (reflectMethod (Proxy @method)) (clientWithRoute pm pa)
    cliHandler _ _ _ = ($)

-- | Same semantics in parsing command line options as 'Verb'.
instance ( RunStreamingClient m
         , MimeUnrender ct chunk
         , ReflectMethod method
         , FramingUnrender framing
         , FromSourceIO chunk a
         ) => HasCLI m (Stream method status framing ct a) ctx where
    type CLIResult  m (Stream method status framing ct a)   = a
    type CLIHandler m (Stream method status framing ct a) r = a -> r
    cliPStructWithContext_ pm pa _ = endpoint (reflectMethod (Proxy @method)) (clientWithRoute pm pa)
    cliHandler _ _ _ = ($)

newtype instance ContextFor m (StreamBody' mods framing ctype a) =
    GenStreamBody { genStreamBody :: m a }

-- | As a part of @ctx@, asks for a streaming source @a@.
instance ( ToSourceIO chunk a
         , MimeRender ctype chunk
         , FramingRender framing
         , StreamBody' mods framing ctype a ∈ ctx
         , HasCLI m api ctx
         , Monad m
         ) => HasCLI m (StreamBody' mods framing ctype a :> api) ctx where
    type CLIResult  m (StreamBody' mods framing ctype a :> api)   = CLIResult m api
    type CLIHandler m (StreamBody' mods framing ctype a :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ p = withParamM (addBody <$> genStreamBody mx)
                     <$> cliPStructWithContext_ pm (Proxy @api) p
      where
        mx :: ContextFor m (StreamBody' mods framing ctype a)
        mx = rget p
        addBody :: a -> Request -> Request
        addBody x = setRequestBody rbs (contentType ctypeP)
          where
            ctypeP   = Proxy @ctype
            framingP = Proxy @framing
#if MIN_VERSION_servant_client_core(0,16,0)
            rbs      = RequestBodySource $
              framingRender framingP
                            (mimeRender ctypeP :: chunk -> BSL.ByteString)
                            (toSourceIO x)
#else
            rbs      = error "HasCLI @StreamBody not supported with servant < 0.16"
#endif

    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | A 'Header'' in the middle of a path is interpreted as a command line
-- argument, prefixed with "header".  For example,
-- @'Servant.API.Header.Header' "foo" 'Int'@ is an option for
-- @--header-foo@.
--
-- Like for 'QueryParam'',  arguments are associated with the action at
-- their endpoint.  After entering all path components and positional
-- arguments, the parser library will begin asking for arguments.
instance ( KnownSymbol sym
         , FromHttpApiData a
         , ToHttpApiData a
         , SBoolI (FoldRequired' 'False mods)
         , Typeable a
         , HasCLI m api ctx
         ) => HasCLI m (Header' mods sym a :> api) ctx where
    type CLIResult  m (Header' mods sym a :> api)   = CLIResult m api
    type CLIHandler m (Header' mods sym a :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ p = opt ?:>
        fmap (.: addParam) (cliPStructWithContext_ pm (Proxy @api) p)
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

    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | Using 'HttpVersion' has no affect on CLI operations.
instance HasCLI m api ctx => HasCLI m (HttpVersion :> api) ctx where
    type CLIResult  m (HttpVersion :> api)   = CLIResult m api
    type CLIHandler m (HttpVersion :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ = cliPStructWithContext_ pm (Proxy @api)
    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | 'Summary' is displayed during @--help@ when it is reached while
-- navigating down subcommands.
instance (KnownSymbol desc, HasCLI m api ctx) => HasCLI m (Summary desc :> api) ctx where
    type CLIResult  m (Summary desc :> api)   = CLIResult m api
    type CLIHandler m (Summary desc :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ = note [symbolVal (Proxy @desc)]
                     . cliPStructWithContext_ pm (Proxy :: Proxy api)
    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | 'Description' is displayed during @--help@ when it is reached while
-- navigating down subcommands.
instance (KnownSymbol desc, HasCLI m api ctx) => HasCLI m (Description desc :> api) ctx where
    type CLIResult  m (Description desc :> api)   = CLIResult m api
    type CLIHandler m (Description desc :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ = note [symbolVal (Proxy @desc)]
                     . cliPStructWithContext_ pm (Proxy :: Proxy api)
    cliHandler pm _ = cliHandler pm (Proxy @api)


-- | Asks for method as a command line argument.  If any 'Verb' exists at
-- the same endpoint, it can only be accessed as an extra @RAW@ subcommand
-- (as if it had an extra path component labeled @"RAW"@).
instance RunClient m => HasCLI m Raw ctx where
    type CLIResult  m Raw   = Response
    type CLIHandler m Raw r = Response -> r

    cliPStructWithContext_ pm pa _ = rawEndpoint . flip $ clientWithRoute pm pa
    cliHandler _ _ _ = ($)

instance HasCLI m api ctx => HasCLI m (Vault :> api) ctx where
    type CLIResult  m (Vault :> api)   = CLIResult m api
    type CLIHandler m (Vault :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ = cliPStructWithContext_ pm (Proxy @api)
    cliHandler pm _ = cliHandler pm (Proxy @api)

instance HasCLI m api ctx => HasCLI m (RemoteHost :> api) ctx where
    type CLIResult  m (RemoteHost :> api)   = CLIResult m api
    type CLIHandler m (RemoteHost :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ = cliPStructWithContext_ pm (Proxy @api)
    cliHandler pm _ = cliHandler pm (Proxy @api)

instance HasCLI m api ctx => HasCLI m (IsSecure :> api) ctx where
    type CLIResult  m (IsSecure :> api)   = CLIResult m api
    type CLIHandler m (IsSecure :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ = cliPStructWithContext_ pm (Proxy @api)
    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | Contains a subcontext that can be descended down into using
-- 'NamedContext'.  Mirrors 'Servant.Server.NamedContext'.
--
-- Useful for when you have multiple items with the same name within
-- a context; this essentially creates a namespace for context items.
newtype NamedContext m (name :: Symbol) (subContext :: [Type])
    = NamedContext (Rec (ContextFor m) subContext)

newtype instance ContextFor m (NamedContext m name subContext)
    = NC (NamedContext m name subContext)

-- | Allows you to access 'NamedContext's inside a context.
descendIntoNamedContext
    :: forall (name :: Symbol) context subContext m. NamedContext m name subContext ∈ context
    => Proxy name
    -> Rec (ContextFor m) context
    -> Rec (ContextFor m) subContext
descendIntoNamedContext _ p = p'
  where
    NC (NamedContext p' :: NamedContext m name subContext) = rget p

-- | Descend down a subcontext indexed by a given name.  Must be provided
-- when parsing within the context.
--
-- Useful for when you have multiple items with the same name within
-- a context; this essentially creates a namespace for context items.
instance ( NamedContext m name subctx ∈ ctx
         , HasCLI m subapi subctx
         ) => HasCLI m (WithNamedContext name subctx subapi) ctx where
    type CLIResult  m (WithNamedContext name subctx subapi)   = CLIResult m subapi
    type CLIHandler m (WithNamedContext name subctx subapi) r = CLIHandler m subapi r

    cliPStructWithContext_ pm _ = cliPStructWithContext_ pm (Proxy @subapi)
                     . descendIntoNamedContext @_ @ctx @subctx (Proxy @name)
    cliHandler pm _ _ = cliHandler pm (Proxy @subapi) (Proxy @subctx)

newtype instance ContextFor m (AuthProtect tag) = GenAuthReq
    { genAuthReq :: m (AuthenticatedRequest (AuthProtect tag))
    }

-- | Add 'GenAuthReq' to the required context, meaning it must be
-- provided to allow the client to generate authentication data.  The
-- action will only be run if the user selects this endpoint via command
-- line arguments.
--
-- Please use a secure connection!
instance ( HasCLI m api ctx
         , AuthProtect tag ∈ ctx
         , Monad m
         ) => HasCLI m (AuthProtect tag :> api) ctx where
    type CLIResult  m (AuthProtect tag :> api)   = CLIResult m api
    type CLIHandler m (AuthProtect tag :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ p = withParamM (uncurry (&) . unAuthReq <$> genAuthReq md)
                     <$> cliPStructWithContext_ pm (Proxy @api) p
      where
        md :: ContextFor m (AuthProtect tag)
        md = rget p

    cliHandler pm _ = cliHandler pm (Proxy @api)

newtype instance ContextFor m (BasicAuth realm usr) = GenBasicAuthData
    { genBasicAuthData :: m BasicAuthData
    }

-- | Add 'GenBasicAuthData' to the required context, meaning it must be
-- provided to allow the client to generate authentication data.  The
-- action will only be run if the user selects this endpoint via command
-- line arguments.
--
-- Please use a secure connection!
instance ( ToAuthInfo (BasicAuth realm usr)
         , HasCLI m api ctx
         , BasicAuth realm usr ∈ ctx
         , Monad m
         ) => HasCLI m (BasicAuth realm usr :> api) ctx where
    type CLIResult  m (BasicAuth realm usr :> api)   = CLIResult m api
    type CLIHandler m (BasicAuth realm usr :> api) r = CLIHandler m api r

    cliPStructWithContext_ pm _ p = note [infonote, reqnote]
                       $ withParamM (basicAuthReq <$> genBasicAuthData md)
                     <$> cliPStructWithContext_ pm (Proxy @api) p
      where
        md :: ContextFor m (BasicAuth realm usr)
        md = rget p
        infonote = "Authentication required: " ++ _authIntro
        reqnote = "Required information: " ++ _authDataRequired

        DocAuthentication{..} = toAuthInfo (Proxy @(BasicAuth realm usr))

    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | Helper for mapping parameter generators
withParamM
    :: Monad m
    => m (a -> a)
    -> (a -> m b)
    -> a
    -> m b
withParamM mf g x = do
    f <- mf
    g (f x)

-- | Two-argument function composition
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)
