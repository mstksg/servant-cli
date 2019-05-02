{-# LANGUAGE AllowAmbiguousTypes   #-}
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

module Servant.CLI.HasCLI (
    HasCLI(..)
  -- * Utility
  , splitRec
  ) where

import           Data.Bifunctor
import           Data.Char
import           Data.Function
import           Data.Kind
import           Data.List
import           Data.Profunctor
import           Data.Proxy
import           Data.Vinyl hiding     (rmap)
import           Data.Vinyl.TypeLevel
import           Data.Void
import           GHC.TypeLits hiding   (Mod)
import           Options.Applicative
import           Servant.API hiding    (addHeader, HList)
import           Servant.API.Modifiers
import           Servant.CLI.ParseBody
import           Servant.CLI.Structure
import           Servant.Client.Core
import           Servant.Docs hiding   (Endpoint, Response)
import           Text.Printf
import           Type.Reflection
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.CaseInsensitive  as CI
import qualified Data.List.NonEmpty    as NE
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T


-- | Typeclass defining how each API combinator influences how a server can
-- be interacted with using command line options.
--
-- Unless you are adding new combinators to be used with APIs, you can
-- ignore this class.
class HasCLI m api where
    -- | The type of the API client action that the command line parser
    -- will return.  In most cases, this will be the return type of the
    -- final endpoint action, or else an 'Either' between two branches.
    -- type CLI (m :: Type -> Type) (api :: Type) :: Type

    type CLIParam   (m :: Type -> Type) (api :: Type) :: [Type]
    type CLIResult  (m :: Type -> Type) (api :: Type) :: Type
    type CLIHandler (m :: Type -> Type) (api :: Type) (r :: Type) :: Type

    -- | Generate a 'PStruct' showing how to modify a 'Request' and perform
    -- an action, given an API and underlying monad.  Only meant for
    -- internal use; should be used through 'Servant.CLI.cliPStruct'
    -- instead.
    cliPStruct_
        :: Proxy m
        -> Proxy api
        -> Rec m (CLIParam m api)
        -> PStruct (Request -> m (CLIResult m api))

    -- | Handle all the possibilities in a 'CLIResult', by giving the
    -- appropriate 'CLIHandler'
    cliHandler
        :: Proxy m
        -> Proxy api
        -> CLIHandler m api r
        -> CLIResult m api
        -> r

-- | 'EmptyAPI' will always fail to parse.
instance HasCLI m EmptyAPI where
    type CLIParam   m EmptyAPI   = '[]
    type CLIResult  m EmptyAPI   = Void
    type CLIHandler m EmptyAPI r = Void -> r

    cliPStruct_ _ _ _ = mempty
    cliHandler  _ _ = ($)

-- | Un-append two 'Rec's.
splitRec
   :: forall as bs g. RecApplicative as
   => Rec g (as ++ bs)
   -> (Rec g as, Rec g bs)
splitRec = go (rpure Proxy)
  where
    go  :: Rec Proxy cs -> Rec g (cs ++ bs) -> (Rec g cs, Rec g bs)
    go = \case
      RNil    -> (RNil,)
      _ :& ps -> \case
        x :& xs -> first (x :&) . go ps $ xs

-- | Using alternation with ':<|>' provides an 'Either' between the two
-- results.
instance (HasCLI m a, HasCLI m b, Functor m, RecApplicative (CLIParam m a)) => HasCLI m (a :<|> b) where
    type CLIParam   m (a :<|> b)   = CLIParam m a ++ CLIParam m b
    type CLIResult  m (a :<|> b)   = Either (CLIResult m a) (CLIResult m b)
    type CLIHandler m (a :<|> b) r = CLIHandler m a r :<|> CLIHandler m b r

    cliPStruct_ pm _ (splitRec->(pA,pB)) =
          dig Left  (cliPStruct_ pm (Proxy @a) pA)
       <> dig Right (cliPStruct_ pm (Proxy @b) pB)
      where
        dig = fmap . rmap . fmap

    cliHandler pm _ (hA :<|> hB) = either (cliHandler pm (Proxy @a) hA)
                                             (cliHandler pm (Proxy @b) hB)

-- | A path component is interpreted as a "subcommand".
instance (KnownSymbol path, HasCLI m api) => HasCLI m (path :> api) where
    type CLIParam   m (path :> api)   = CLIParam m api
    type CLIResult  m (path :> api)   = CLIResult m api
    type CLIHandler m (path :> api) r = CLIHandler m api r

    cliPStruct_ pm _ p = pathstr $:>
        (fmap . lmap) (appendToPath (T.pack pathstr)) (cliPStruct_ pm (Proxy @api) p)
      where
        pathstr = symbolVal (Proxy @path)

    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | A 'Capture' is interpreted as a positional required command line argument.
instance ( FromHttpApiData a
         , ToHttpApiData a
         , Typeable a
         , ToCapture (Capture sym a)
         , HasCLI m api
         ) => HasCLI m (Capture' mods sym a :> api) where
    type CLIParam   m (Capture' mods sym a :> api)   = CLIParam m api
    type CLIResult  m (Capture' mods sym a :> api)   = CLIResult m api
    type CLIHandler m (Capture' mods sym a :> api) r = CLIHandler m api r

    cliPStruct_ pm _ p = arg #:>
        (fmap . flip) (lmap . addCapture) (cliPStruct_ pm (Proxy @api) p)
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

    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | A 'CaptureAll' is interpreted as arbitrarily many command line
-- arguments.  If there is more than one final endpoint method, the method
-- must be given as a command line option before beginning the arguments.
instance ( FromHttpApiData a
         , ToHttpApiData a
         , Typeable a
         , ToCapture (CaptureAll sym a)
         , HasCLI m api
         ) => HasCLI m (CaptureAll sym a :> api) where
    type CLIParam   m (CaptureAll sym a :> api)   = CLIParam m api
    type CLIResult  m (CaptureAll sym a :> api)   = CLIResult m api
    type CLIHandler m (CaptureAll sym a :> api) r = CLIHandler m api r

    cliPStruct_ pm _ p = arg ##:>
        (fmap . flip) (lmap . addCapture) (cliPStruct_ pm (Proxy @api) p)
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

    cliHandler pm _ = cliHandler pm (Proxy @api)

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
    type CLIParam   m (QueryParam' mods sym a :> api)   = CLIParam m api
    type CLIResult  m (QueryParam' mods sym a :> api)   = CLIResult m api
    type CLIHandler m (QueryParam' mods sym a :> api) r = CLIHandler m api r

    cliPStruct_ pm _ p = opt ?:>
        (fmap . flip) (lmap . addParam) (cliPStruct_ pm (Proxy @api) p)
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

    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | Query flags are interpreted as command line flags/switches.
--
-- 'QueryFlag' arguments are associated with the action at their endpoint.
-- After entering all path components and positional arguments, the parser
-- library will begin asking for arguments.
instance ( KnownSymbol sym
         , ToParam (QueryFlag sym)
         , HasCLI m api
         ) => HasCLI m (QueryFlag sym :> api) where
    type CLIParam   m (QueryFlag sym :> api)   = CLIParam m api
    type CLIResult  m (QueryFlag sym :> api)   = CLIResult m api
    type CLIHandler m (QueryFlag sym :> api) r = CLIHandler m api r

    cliPStruct_ pm _ p = opt ?:>
        (fmap . flip) (lmap . addParam) (cliPStruct_ pm (Proxy @api) p)
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
         , HasCLI m api
         ) => HasCLI m (ReqBody' mods (ct ': cts) a :> api) where
    type CLIParam   m (ReqBody' mods (ct ': cts) a :> api)   = CLIParam m api
    type CLIResult  m (ReqBody' mods (ct ': cts) a :> api)   = CLIResult m api
    type CLIHandler m (ReqBody' mods (ct ': cts) a :> api) r = CLIHandler m api r

    cliPStruct_ pm _ p = parseBody @a %:>
        (fmap . flip) (lmap . addBody) (cliPStruct_ pm (Proxy @api) p)
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
         ) => HasCLI m (Verb method status cts' a) where
    type CLIParam   m (Verb method status cts' a)   = '[]
    type CLIResult  m (Verb method status cts' a)   = a
    type CLIHandler m (Verb method status cts' a) r = a -> r

    cliPStruct_ pm pa _ = endpoint (reflectMethod (Proxy @method)) (clientWithRoute pm pa)
    cliHandler _ _ = ($)

instance ( RunStreamingClient m
         , MimeUnrender ct chunk
         , ReflectMethod method
         , FramingUnrender framing
         , FromSourceIO chunk a
         ) => HasCLI m (Stream method status framing ct a) where
    type CLIParam   m (Stream method status framing ct a)   = '[]
    type CLIResult  m (Stream method status framing ct a)   = a
    type CLIHandler m (Stream method status framing ct a) r = a -> r
    cliPStruct_ pm pa _ = endpoint (reflectMethod (Proxy @method)) (clientWithRoute pm pa)
    cliHandler _ _ = ($)

-- | The final action will require a streaming source @a@ to be given.
-- Instead of normally parsing command line arguments into an @m a@, it
-- parses it into a @src -> m a@, awaiting a streaming source to send as
-- body.
instance ( ToSourceIO chunk a
         , MimeRender ctype chunk
         , FramingRender framing
         , HasCLI m api
         , Monad m
         ) => HasCLI m (StreamBody' mods framing ctype a :> api) where
    type CLIParam   m (StreamBody' mods framing ctype a :> api)   = a ': CLIParam m api
    type CLIResult  m (StreamBody' mods framing ctype a :> api)   = CLIResult m api
    type CLIHandler m (StreamBody' mods framing ctype a :> api) r = CLIHandler m api r

    cliPStruct_ pm _ (mx :& p) =
        withParamM (addBody <$> mx) <$> cliPStruct_ pm (Proxy @api) p
      where
        addBody x = setRequestBody (RequestBodySource sourceIO) (contentType ctypeP)
          where
            ctypeP   = Proxy @ctype
            framingP = Proxy @framing
            sourceIO = framingRender
                framingP
                (mimeRender ctypeP :: chunk -> BSL.ByteString)
                (toSourceIO x)

    cliHandler pm _ = cliHandler pm (Proxy @api)

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
    type CLIParam   m (Header' mods sym a :> api)   = CLIParam m api
    type CLIResult  m (Header' mods sym a :> api)   = CLIResult m api
    type CLIHandler m (Header' mods sym a :> api) r = CLIHandler m api r

    cliPStruct_ pm _ p = opt ?:>
        (fmap . flip) (lmap . addParam) (cliPStruct_ pm (Proxy @api) p)
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
instance HasCLI m api => HasCLI m (HttpVersion :> api) where
    type CLIParam   m (HttpVersion :> api)   = CLIParam m api
    type CLIResult  m (HttpVersion :> api)   = CLIResult m api
    type CLIHandler m (HttpVersion :> api) r = CLIHandler m api r

    cliPStruct_ pm _ = cliPStruct_ pm (Proxy @api)
    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | 'Summary' is displayed during @--help@ when it is reached while
-- navigating down subcommands.
instance (KnownSymbol desc, HasCLI m api) => HasCLI m (Summary desc :> api) where
    type CLIParam   m (Summary desc :> api)   = CLIParam m api
    type CLIResult  m (Summary desc :> api)   = CLIResult m api
    type CLIHandler m (Summary desc :> api) r = CLIHandler m api r

    cliPStruct_ pm _ = note (symbolVal (Proxy @desc))
                     . cliPStruct_ pm (Proxy :: Proxy api)
    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | 'Description' is displayed during @--help@ when it is reached while
-- navigating down subcommands.
instance (KnownSymbol desc, HasCLI m api) => HasCLI m (Description desc :> api) where
    type CLIParam   m (Description desc :> api)   = CLIParam m api
    type CLIResult  m (Description desc :> api)   = CLIResult m api
    type CLIHandler m (Description desc :> api) r = CLIHandler m api r

    cliPStruct_ pm _ = note (symbolVal (Proxy @desc))
                     . cliPStruct_ pm (Proxy :: Proxy api)
    cliHandler pm _ = cliHandler pm (Proxy @api)


-- | Asks for method as a command line argument.  If any 'Verb' exists at
-- the same endpoint, it can only be accessed as an extra @RAW@ subcommand
-- (as if it had an extra path component labeled @"RAW"@).
instance RunClient m => HasCLI m Raw where
    type CLIParam   m Raw   = '[]
    type CLIResult  m Raw   = Response
    type CLIHandler m Raw r = Response -> r

    cliPStruct_ pm pa _ = rawEndpoint . flip $ clientWithRoute pm pa
    cliHandler _ _ = ($)

instance HasCLI m api => HasCLI m (Vault :> api) where
    type CLIParam   m (Vault :> api)   = CLIParam m api
    type CLIResult  m (Vault :> api)   = CLIResult m api
    type CLIHandler m (Vault :> api) r = CLIHandler m api r

    cliPStruct_ pm _ = cliPStruct_ pm (Proxy @api)
    cliHandler pm _ = cliHandler pm (Proxy @api)

instance HasCLI m api => HasCLI m (RemoteHost :> api) where
    type CLIParam   m (RemoteHost :> api)   = CLIParam m api
    type CLIResult  m (RemoteHost :> api)   = CLIResult m api
    type CLIHandler m (RemoteHost :> api) r = CLIHandler m api r

    cliPStruct_ pm _ = cliPStruct_ pm (Proxy @api)
    cliHandler pm _ = cliHandler pm (Proxy @api)

instance HasCLI m api => HasCLI m (IsSecure :> api) where
    type CLIParam   m (IsSecure :> api)   = CLIParam m api
    type CLIResult  m (IsSecure :> api)   = CLIResult m api
    type CLIHandler m (IsSecure :> api) r = CLIHandler m api r

    cliPStruct_ pm _ = cliPStruct_ pm (Proxy @api)
    cliHandler pm _ = cliHandler pm (Proxy @api)

instance HasCLI m subapi => HasCLI m (WithNamedContext name context subapi) where
    type CLIParam   m (WithNamedContext name context subapi)   = CLIParam m subapi
    type CLIResult  m (WithNamedContext name context subapi)   = CLIResult m subapi
    type CLIHandler m (WithNamedContext name context subapi) r = CLIHandler m subapi r

    cliPStruct_ pm _ = cliPStruct_ pm (Proxy @subapi)
    cliHandler pm _ = cliHandler pm (Proxy @subapi)

-- | Adding 'AuthProtect' means that the result of parsing command line
-- options will be a function that takes a 'AuthenticatedRequest' and
-- returns an @m@ action to make a request to the server with that
-- authentication data.
--
-- Please use a secure connection!
instance (HasCLI m api, Monad m) => HasCLI m (AuthProtect tag :> api) where
    type CLIParam   m (AuthProtect tag :> api)   = AuthenticatedRequest (AuthProtect tag)
                                                ': CLIParam m api
    type CLIResult  m (AuthProtect tag :> api)   = CLIResult m api
    type CLIHandler m (AuthProtect tag :> api) r = CLIHandler m api r

    cliPStruct_ pm _ (d :& p) =
        withParamM (uncurry (&) . unAuthReq <$> d) <$> cliPStruct_ pm (Proxy @api) p

    cliHandler pm _ = cliHandler pm (Proxy @api)

-- | Adding 'BasicAuth' means that the result of parsing command line
-- options will be a function that takes a 'BasicAuthData' and returns an
-- @m@ action to make a request to the server with that authentication
-- data.
--
-- Please use a secure connection!
instance (HasCLI m api, Monad m) => HasCLI m (BasicAuth realm usr :> api) where
    type CLIParam   m (BasicAuth realm usr :> api)   = BasicAuthData
                                                    ': CLIParam m api
    type CLIResult  m (BasicAuth realm usr :> api)   = CLIResult m api
    type CLIHandler m (BasicAuth realm usr :> api) r = CLIHandler m api r

    cliPStruct_ pm _ (d :& p) =
        withParamM (basicAuthReq <$> d) <$> cliPStruct_ pm (Proxy @api) p

    cliHandler pm _ = cliHandler pm (Proxy @api)

withParamM
    :: Monad m
    => m (a -> a)
    -> (a -> m b)
    -> a
    -> m b
withParamM mf g x = do
    f <- mf
    g (f x)

