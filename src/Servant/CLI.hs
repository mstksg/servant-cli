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
    { argDesc :: String
    , argMeta :: String
    , argRead :: ReadM a
    }
  deriving Functor

data MultiArg :: Type -> Type where
    MultiArg :: Arg a -> MultiArg [a]

type Captures = Day Arg      PStruct
            :+: Day MultiArg (Map HTTP.Method :.: Endpoint)

-- | Endpoint arguments and body.
data Endpoint a = Endpoint
    { epStruct :: Day (Ap Opt) Parser a }
  deriving Functor

-- | Structure for a parser of a given value that may use items from
-- captures and arguments.
data PStruct a = PStruct
    { psComponent :: Map String (PStruct a)
    , psCaptures  :: Maybe (Captures a)
    , psEndpoints :: Map HTTP.Method (Endpoint a)
    }
  deriving Functor

makeBaseFunctor ''PStruct

(|+|) :: (f a -> r) -> (g a -> r) -> (f :+: g) a -> r
f |+| g = \case
    L1 x -> f x
    R1 y -> g y

mapInnerComp :: Functor f => (g a -> g b) -> (f :.: g) a -> (f :.: g) b
mapInnerComp f (Comp1 x) = Comp1 (fmap f x)

structParser :: PStruct a -> Parser a
structParser = cata go
  where
    go :: PStructF x (Parser x) -> Parser x
    go (PStructF cs c eps) = subp <|> cap <|> ep
      where
        (Any anySubs, subs) = M.foldMapWithKey mkCmd $ cs
        subp
          | anySubs   = subparser subs
          | otherwise = empty
        cap   = maybe empty (mkArg |+| mkArgs) c
        ep    = methodPicker eps
    mkCmd :: String -> Parser x -> (Any, Mod CommandFields x)
    mkCmd c p = (Any True, command c $ info (p <**> helper) mempty)
    mkArg :: Day Arg PStruct x -> Parser x
    mkArg (Day a p f) = f <$> argParser a <*> structParser p
    mkArgs :: Day MultiArg (Map HTTP.Method :.: Endpoint) x -> Parser x
    mkArgs (Day (MultiArg a) (Comp1 ps) f) =
            flip f <$> methodPicker ps
                   <*> many (argParser a)
    argParser :: Arg x -> Parser x
    argParser Arg{..} = argument argRead $ help argDesc
                                        <> metavar argMeta
    mkEndpoint :: Endpoint x -> Parser x
    mkEndpoint (Endpoint (Day o b f)) = f <$> runAp mkOpt o
                                          <*> b
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
    pickMethod :: BS.ByteString -> Parser x -> Mod CommandFields x
    pickMethod m p = command (T.unpack . T.decodeUtf8 $ m) $ info (p <**> helper) mempty

-- -- | Structure for a parser of a given value that may use items from
-- -- captures and arguments.
-- data PStruct :: Type -> Type where
--     PStruct   :: Map String (PStruct a)       -- ^ more components
--               -> Maybe (Captures a)           -- ^ capture
--               -> Map HTTP.Method (Endpoint a) -- ^ endpoints
--               -> PStruct a
--   deriving Functor


altPStruct :: PStruct a -> PStruct a -> PStruct a
altPStruct (PStruct cs1 c1 ep1) (PStruct cs2 c2 ep2) = PStruct cs3 c3 ep3
  where
    cs3 = case c1 of
      Just _  -> cs1
      Nothing -> M.unionWith altPStruct cs1 cs2
    c3  = c1 <|> c2
    ep3 = M.unionWith const ep1 ep2

instance Semigroup (PStruct a) where
    (<>) = altPStruct

instance Monoid (PStruct a) where
    mempty = PStruct M.empty Nothing M.empty

branch :: PStruct a -> PStruct b -> PStruct (Either a b)
branch x y = (Left <$> x) `altPStruct` (Right <$> y)

infixr 3 `branch`

($:>) :: String -> PStruct a -> PStruct a
c $:> p = PStruct (M.singleton c p) Nothing M.empty
infixr 4 $:>

(?:>) :: Opt a -> PStruct (a -> b) -> PStruct b
o ?:> PStruct cs c ep = PStruct cs' c' ep'
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

(#:>) :: Arg a -> PStruct (a -> b) -> PStruct b
a #:> p = PStruct M.empty (Just (L1 (Day a p (&)))) M.empty
infixr 4 #:>

(##:>) :: Arg a -> PStruct ([a] -> b) -> PStruct b
a ##:> p = PStruct M.empty
                   (Just (R1 (Day (MultiArg a) (Comp1 (psEndpoints p)) (&))))
                   M.empty
infixr 4 ##:>

-- | Add a request body.  NOTE!!!!! UNDEFINED BEHAVIOR IF DONE MORE THAN
-- ONCE?
(%:>) :: Parser a -> PStruct (a -> b) -> PStruct b
b %:> PStruct cs c ep = PStruct cs' c' ep'
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
endpoint m = PStruct M.empty Nothing . M.singleton m . Endpoint . pure

orRequired :: ReadM a -> Coyoneda OptRead a
orRequired = liftCoyoneda . ORRequired

orOptional :: ReadM a -> Coyoneda OptRead (Maybe a)
orOptional = liftCoyoneda . OROptional

orSwitch :: Coyoneda OptRead Bool
orSwitch = liftCoyoneda ORSwitch

defaultParseBody :: forall a. Typeable a => ReadM a -> Parser a
defaultParseBody r = option r
    ( metavar (map toUpper tp)
   <> long "data"
   <> short 'd'
   <> help (printf "Request body (%s)" tp)
    )
  where
    tp = show (typeRep @a)

testStruct :: PStruct (Either (Bool, String) String)
testStruct = hello `branch` (greetDelete <> greetPost)
  where
    hello       = "hello"
              $:> Arg "name" "<name>" (str @String)
              #:> Opt "capital" "capital" "BOOL" orSwitch
              ?:> endpoint HTTP.methodGet (,)
    greetPost   = "greet"
              $:> defaultParseBody (str @String)
              %:> endpoint HTTP.methodPost (map toUpper)
    greetDelete = "greet"
              $:> Arg "greetid" "<greetid>" (str @String)
              #:> endpoint HTTP.methodDelete (map toLower)

-- class (HasDocs api, HasClient m api) => HasCLI m api where
--     type CLI m api

--     clientParser_
--         :: Proxy m
--         -> Proxy api
--         -> Client m api
--         -> Parser (m (CLI m api))

-- instance HasClient m EmptyAPI => HasCLI m EmptyAPI where
--     type CLI m EmptyAPI = EmptyClient
--     clientParser_ _ _ = pure . pure

-- instance (HasCLI m a, HasCLI m b) => HasCLI m (a :<|> b) where
--     type CLI m (a :<|> b) = Either (CLI m a) (CLI m b)
--     clientParser_ pm _ (cA :<|> cB) = (fmap Left  <$> clientParser_ pm (Proxy @a) cA)
--                                  <|> (fmap Right <$> clientParser_ pm (Proxy @b) cB)

-- instance (KnownSymbol path, HasCLI m api) => HasCLI m (path :> api) where
--     type CLI m (path :> api) = CLI m api
--     clientParser_ pm _ api = subparser $
--          command pathstr ( info (clientParser_ pm (Proxy @api) api <**> helper) mempty)
--       <> metavar pathstr
--       where
--         pathstr = symbolVal (Proxy @path)

-- instance ( KnownSymbol sym
--          , FromHttpApiData a
--          , ToHttpApiData a
--          , Typeable a
--          , ToCapture (Capture sym a)
--          , HasCLI m api
--          ) => HasCLI m (Capture' mods sym a :> api) where
--     type CLI m (Capture' mods sym a :> api) = CLI m api
--     clientParser_ pm _ api = BindP arg $ clientParser_ pm (Proxy @api) . api
--       where
--         arg :: Parser a
--         arg = argument (eitherReader (first T.unpack . parseUrlPiece @a . T.pack))
--                 ( metavar (map toUpper _capSymbol)
--                <> help (printf "%s (%s)" _capDesc capType)
--                 )
--         capType = show $ typeRep @a
--         DocCapture{..} = toCapture (Proxy @(Capture sym a))

-- instance ( KnownSymbol sym
--          , FromHttpApiData a
--          , ToHttpApiData a
--          , SBoolI (FoldRequired' 'False mods)
--          , Typeable a
--          , ToParam (QueryParam' mods sym a)
--          , HasCLI m api
--          ) => HasCLI m (QueryParam' mods sym a :> api) where
--     type CLI m (QueryParam' mods sym a :> api) = CLI m api
--     clientParser_ pm _ api = BindP opt' $ clientParser_ pm (Proxy @api) . api
--       where
--         opt :: Parser a
--         opt = option (eitherReader (first T.unpack . parseUrlPiece @a . T.pack))
--                 ( metavar (map toUpper pType)
--                <> long pName
--                <> help (printf "%s (%s)" _paramDesc pType)
--                 )
--         opt' :: Parser (If (FoldRequired' 'False mods) a (Maybe a))
--         opt' = case sbool @(FoldRequired' 'False mods) of
--           STrue  -> opt
--           SFalse -> optional opt
--         pType = show $ typeRep @a
--         pName = symbolVal (Proxy @sym)
--         DocQueryParam{..} = toParam (Proxy @(QueryParam' mods sym a))
--         -- TODO: experiment with more detailed help doc
--         -- also, we can offer completion with values

-- instance ( KnownSymbol sym
--          , ToParam (QueryFlag sym)
--          , HasCLI m api
--          ) => HasCLI m (QueryFlag sym :> api) where
--     type CLI m (QueryFlag sym :> api) = CLI m api
--     clientParser_ pm _ api = BindP opt $ clientParser_ pm (Proxy @api) . api
--       where
--         opt :: Parser Bool
--         opt = switch ( long (symbolVal (Proxy @sym)) )

-- class ParseBody a where
--     parseBody :: Parser a

--     default parseBody :: (Typeable a, Read a) => Parser a
--     parseBody = defaultParseBody auto

-- instance ( MimeRender ct a
--          , ParseBody a
--          , ToSample a
--          , AllMimeRender (ct ': cts) a
--          , HasCLI m api
--          ) => HasCLI m (ReqBody' mods (ct ': cts) a :> api) where
--     type CLI m (ReqBody' mods (ct ': cts) a :> api) = CLI m api
--     clientParser_ pm _ api =
--         BindP parseBody $
--           clientParser_ pm (Proxy @api) . api
--     -- TODO: use tosample to provide samples?

-- -- TODO: we have a problem here, how to distinguish DELETE and POST
-- -- requests with the same name.
-- instance {-# OVERLAPPABLE #-}
--     -- Note [Non-Empty Content Types]
--     ( RunClient m, MimeUnrender ct a, ReflectMethod method, cts' ~ (ct ': cts)
--     , ToSample a
--     , AllMimeRender (ct ': cts) a
--     , KnownNat status
--     ) => HasCLI m (Verb method status cts' a) where
--     type CLI m (Verb method status cts' a) = a
--     clientParser_ _ _ = pure

-- instance {-# OVERLAPPING #-}
--     ( RunClient m, ReflectMethod method
--     , HasDocs (Verb method status cts NoContent)
--     ) => HasCLI m (Verb method status cts NoContent) where
--     type CLI m (Verb method status cts NoContent) = NoContent
--     clientParser_ _ _ = pure

-- instance {-# OVERLAPPING #-}
--     -- Note [Non-Empty Content Types]
--     ( RunClient m, MimeUnrender ct a, BuildHeadersTo ls
--     , ReflectMethod method, cts' ~ (ct ': cts)
--     , ToSample a
--     , AllMimeRender (ct ': cts) a
--     , KnownNat status
--     , AllHeaderSamples ls
--     , GetHeaders (HList ls)
--     ) => HasCLI m (Verb method status cts' (Headers ls a)) where
--     type CLI m (Verb method status cts' (Headers ls a)) = Headers ls a
--     clientParser_ _ _ = pure

-- instance {-# OVERLAPPING #-}
--     ( RunClient m, BuildHeadersTo ls, ReflectMethod method
--     , HasDocs (Verb method status cts (Headers ls NoContent))
--     ) => HasCLI m (Verb method status cts (Headers ls NoContent)) where
--     type CLI m (Verb method status cts (Headers ls NoContent))
--       = Headers ls NoContent
--     clientParser_ _ _ = pure

-- clientParser
--     :: HasCLI m api
--     => Proxy api
--     -> Proxy m
--     -> Parser (m (CLI m api))
-- clientParser papi pm = clientParser_ pm papi (clientIn papi pm)

-- parseClient
--     :: HasCLI ClientM api
--     => Proxy api
--     -> IO (ClientM (CLI ClientM api))
-- parseClient p = execParser $ info (clientParser p (Proxy @ClientM) <**> helper) mempty




-- defaultParseBody :: forall a. Typeable a => ReadM a -> Parser a
-- defaultParseBody r = option r
--     ( metavar (map toUpper tp)
--    <> long "data"
--    <> short 'd'
--    <> help (printf "Request body (%s)" tp)
--     )
--   where
--     tp = show (typeRep @a)

-- instance ParseBody String where
--     parseBody = defaultParseBody str

-- instance ParseBody T.Text where
--     parseBody = defaultParseBody str

-- instance ParseBody TL.Text where
--     parseBody = defaultParseBody str

-- instance ParseBody Int where
-- instance ParseBody Integer where
-- instance ParseBody Float where
-- instance ParseBody Double where
