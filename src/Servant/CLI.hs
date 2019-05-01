{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.CLI where

-- module Servant.CLI (
--     -- HasClient(..)
--   -- , clientParser
--   -- , parseClient
--   -- , ParseBody(..)
--   -- , defaultParseBody
--   -- -- * Re-export
--   -- , ToSample(..)
--   -- , ToCapture(..), DocCapture(..)
--   -- , ToParam(..), DocQueryParam(..)
--   ) where

-- import           Data.Singletons.Prelude.List
-- import           Servant.Server.Internal
import           Control.Alternative.Free
import           Control.Applicative
import           Control.Applicative.Free
import           Control.Applicative.Lift
import           Control.Monad.Free
import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.Functor.Coyoneda
import           Data.Functor.Day
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.Functor.Identity
import           Data.Functor.Yoneda
import           Data.Kind
import           Data.Map                                  (Map)
import           Data.Profunctor
import           Data.Proxy
import           Data.Semigroup hiding                     (Option(..), option, Arg(..))
import           Data.Singletons
import           Data.Type.Predicate.Quantification hiding (Any)
import           Data.Vinyl
import           Debug.Trace
import           GHC.Generics
import           GHC.TypeLits hiding                       (Mod)
import           Options.Applicative
import           Options.Applicative.Types
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.API.Modifiers
import           Servant.Client
import           Servant.Client.Core
import           Servant.Docs hiding                       (Endpoint)
import           Servant.Docs.Internal hiding              (Endpoint(..))
import           System.FilePath
import           Text.Printf
import           Type.Reflection
import qualified Control.Alternative.Free                  as Alt
import qualified Control.Monad.Free                        as FM
import qualified Data.ByteString                           as BS
import qualified Data.Map                                  as M
import qualified Data.Monoid                               as Mo
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as T
import qualified Data.Text.Lazy                            as TL
import qualified Network.HTTP.Types                        as HTTP
import qualified Options.Applicative.Help.Pretty           as O

-- | How to "read" an option.
data OptRead :: Type -> Type where
    ORRequired :: ReadM a -> OptRead a
    OROptional :: ReadM a -> OptRead (Maybe a)
    ORSwitch   :: OptRead Bool

-- | Query parameters are interpreted as options
data Opt a = Opt
    { optName :: String
    , optDesc :: String
    , optMeta :: String
    , optRead :: Coyoneda OptRead a
    }
  deriving Functor

-- | Captures are interpreted as arguments
data Arg a = Arg
    { argName :: String
    , argDesc :: String
    , argMeta :: String
    , argRead :: ReadM a
    }
  deriving Functor

data MultiArg :: Type -> Type where
    MultiArg :: Arg a -> MultiArg [a]

type Captures = Day Arg      PStruct
            :+: Day MultiArg (Map HTTP.Method :.: Endpoint)

-- | Endpoint arguments and body.
--
-- TODO: add things like status etc.
data Endpoint a = Endpoint
    { epStruct :: Day (Ap Opt) Parser a }
  deriving Functor

-- | Structure for a parser of a given value that may use items from
-- captures and arguments.
data PStruct a = PStruct
    { psInfo       :: [String]
    , psComponents :: Map String (PStruct a)         -- ^ path components
    , psCaptures   :: Maybe (Captures a)             -- ^ captures
    , psEndpoints  :: Map HTTP.Method (Endpoint a)   -- ^ endpoints
    }
  deriving Functor

makeBaseFunctor ''PStruct

(|+|) :: (f a -> r) -> (g a -> r) -> (f :+: g) a -> r
f |+| g = \case
    L1 x -> f x
    R1 y -> g y

mapInnerComp :: Functor f => (g a -> g b) -> (f :.: g) a -> (f :.: g) b
mapInnerComp f (Comp1 x) = Comp1 (fmap f x)

structParser :: PStruct a -> ParserInfo a
structParser = ($ []) . ($ True) . structParser_

structParser_
    :: PStruct a
    -> Bool         -- ^ add helper
    -> [String]     -- ^ root path
    -> ParserInfo a
structParser_ = cata go
  where
    go :: PStructF x (Bool -> [String] -> ParserInfo x) -> Bool -> [String] -> ParserInfo x
    go (PStructF ns cs c eps) toHelp p = info ((subp <|> cap <|> ep) <**> mkHelp) $
           fullDesc
        <> header (joinPath p)
        <> progDescDoc (Just (O.vcat . map O.string $ ns'))
      where
        (Any anySubs, subs) = M.foldMapWithKey (mkCmd p) $ cs
        subp
          | anySubs   = subparser subs
          | otherwise = empty
        (nsc, cap) = maybe ([], empty) (mkArg p |+| (([],) . mkArgs)) c
        ep         = methodPicker eps
        ns' = ns ++ nsc
        mkHelp
          | toHelp    = helper
          | otherwise = pure id
    mkCmd :: [String] -> String -> (Bool -> [String] -> ParserInfo x) -> (Any, Mod CommandFields x)
    mkCmd ps c p = (Any True, command c (p True (ps ++ [c])))
    mkArg :: [String] -> Day Arg PStruct x -> ([String], Parser x)
    mkArg ps (Day a p f) =
          ( []
          , f <$> argParser a
              <*> infoParser (structParser_ p False (ps ++ [':' : argName a]))
          )
    mkArgs :: Day MultiArg (Map HTTP.Method :.: Endpoint) x -> Parser x
    mkArgs (Day (MultiArg a) (Comp1 ps) f) =
            flip f <$> methodPicker ps
                   <*> many (argParser a)
    argParser :: Arg x -> Parser x
    argParser Arg{..} = argument argRead $ help argDesc
                                        <> metavar argMeta
    mkOpt :: Opt x -> Parser x
    mkOpt Opt{..} = lowerCoyoneda $ (`hoistCoyoneda` optRead) $ \case
        ORRequired r -> option r mods
        OROptional r -> optional $ option r mods
        ORSwitch     -> switch   $ long optName <> help optDesc
      where
        mods :: Mod OptionFields y
        mods = long optName
            <> help optDesc
            <> metavar optMeta
    methodPicker :: Map HTTP.Method (Endpoint x) -> Parser x
    methodPicker eps = case M.minView epMap of
        Nothing       -> empty
        Just (m0, ms)
          | M.null ms -> m0
          | otherwise -> subparser $ M.foldMapWithKey pickMethod epMap
      where
        epMap = mkEndpoint <$> eps
    mkEndpoint :: Endpoint x -> Parser x
    mkEndpoint (Endpoint (Day o b f)) = f <$> runAp mkOpt o
                                          <*> b
    pickMethod :: BS.ByteString -> Parser x -> Mod CommandFields x
    pickMethod m p = command (T.unpack . T.decodeUtf8 $ m) $ info (p <**> helper) mempty

-- | Combine two 'PStruct's, preferring the left hand side for conflicts.
-- If the left hand has a capture, the right hand's components are ignored.
altPStruct :: PStruct a -> PStruct a -> PStruct a
altPStruct (PStruct ns1 cs1 c1 ep1) (PStruct ns2 cs2 c2 ep2) = PStruct ns3 cs3 c3 ep3
  where
    ns3 = ns1 ++ ns2    -- ??
    cs3 = case c1 of
      Just _  -> cs1
      Nothing -> M.unionWith altPStruct cs1 cs2
    c3  = c1 <|> c2
    ep3 = M.unionWith const ep1 ep2

instance Semigroup (PStruct a) where
    (<>) = altPStruct

instance Monoid (PStruct a) where
    mempty = PStruct [] M.empty Nothing M.empty

branch :: PStruct a -> PStruct b -> PStruct (Either a b)
branch x y = (Left <$> x) `altPStruct` (Right <$> y)

infixr 3 `branch`

($:>) :: String -> PStruct a -> PStruct a
c $:> p = mempty { psComponents = M.singleton c p }
infixr 4 $:>

(?:>) :: Opt a -> PStruct (a -> b) -> PStruct b
o ?:> PStruct ns cs c ep = PStruct ns cs' c' ep'
  where
    cs' = (o ?:>) <$> cs
    c'  = c <&> \case
        L1 (Day a p f) ->
          let f' x y z = f z x y
          in  L1 $ Day a (o ?:> (f' <$> p)) (&)
        R1 (Day a p f) ->
          let f' x y z = f z x y
          in  R1 $ Day a (addEndpointOpt o `mapInnerComp` (f' <$> p)) (&)
    ep' = addEndpointOpt o <$> ep
infixr 4 ?:>

addEndpointOpt :: Opt a -> Endpoint (a -> b) -> Endpoint b
addEndpointOpt o (Endpoint (Day eo eb ef)) =
    Endpoint (Day ((,) <$> liftAp o <*> eo) eb $ \(x, y) z -> ef y z x)

note :: String -> PStruct a -> PStruct a
note n (PStruct ns cs c ep) = PStruct (ns ++ [n]) cs c ep
infixr 4 `note`

(#:>) :: Arg a -> PStruct (a -> b) -> PStruct b
a #:> p = mempty { psCaptures = Just (L1 (Day a p (&))) }
infixr 4 #:>

(##:>) :: Arg a -> PStruct ([a] -> b) -> PStruct b
a ##:> p = mempty { psCaptures = Just (R1 (Day (MultiArg a) (Comp1 (psEndpoints p)) (&))) }
infixr 4 ##:>

-- | Add a request body.  NOTE!!!!! UNDEFINED BEHAVIOR IF DONE MORE THAN
-- ONCE?
(%:>) :: Parser a -> PStruct (a -> b) -> PStruct b
b %:> PStruct ns cs c ep = PStruct ns cs' c' ep'
  where
    cs' = (b %:>) <$> cs
    c'  = c <&> \case
        L1 (Day a p f) ->
          let f' x y z = f z x y
          in  L1 $ Day a (b             %:> (f' <$> p)) (&)
        R1 (Day a p f) ->
          let f' x y z = f z x y
          in  R1 $ Day a (addEndpointBody b `mapInnerComp` (f' <$> p)) (&)
    ep' = addEndpointBody b <$> ep
infixr 4 %:>

addEndpointBody :: Parser a -> Endpoint (a -> b) -> Endpoint b
addEndpointBody b (Endpoint (Day eo eb ef)) =
    Endpoint (Day eo (liftA2 (,) b eb) $ \x (y, z) -> ef x z y)


endpoint :: HTTP.Method -> a -> PStruct a
endpoint m x = mempty { psEndpoints = M.singleton m (Endpoint (pure x)) }

orRequired :: ReadM a -> Coyoneda OptRead a
orRequired = liftCoyoneda . ORRequired

orOptional :: ReadM a -> Coyoneda OptRead (Maybe a)
orOptional = liftCoyoneda . OROptional

orSwitch :: Coyoneda OptRead Bool
orSwitch = liftCoyoneda ORSwitch

testStruct :: PStruct (Either (Bool, String) String)
testStruct = note "test program" $ hello `branch` (greetDelete <> greetPost <> whatever)
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
