{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
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
import           Control.Applicative.Free
import           Control.Monad.Free
import           Data.Bifunctor
import           Data.Char
import           Data.Function
import           Data.Functor
import           Data.Functor.Coyoneda
import           Data.Functor.Day
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.Functor.Yoneda
import           Data.Kind
import           Data.Map                                  (Map)
import           Data.Proxy
import           Data.Semigroup hiding                     (Option(..), option, Arg(..))
import           Data.Singletons
import           Data.Type.Predicate.Quantification hiding (Any)
import           Data.Vinyl
import           GHC.Generics
import           GHC.TypeLits hiding                       (Mod)
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
import qualified Control.Alternative.Free                  as Alt
import qualified Control.Monad.Free                        as FM
import qualified Data.ByteString                           as BS
import qualified Data.Map                                  as M
import qualified Data.Monoid                               as Mo
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as T
import qualified Data.Text.Lazy                            as TL
import qualified Network.HTTP.Types                        as HTTP

data OptRead :: Type -> Type where
    ORRequired :: ReadM a -> OptRead a
    OROptional :: ReadM a -> OptRead (Maybe a)
    ORSwitch   :: OptRead Bool

-- | Query parameters are interpreted as Opt
data Opt a = Opt
    { optName :: String
    , optDesc :: String
    , optMeta :: String
    , optRead :: Coyoneda OptRead a
    }
  deriving Functor

data Arg a = Arg
    { argDesc :: String
    , argMeta :: String
    , argRead :: ReadM a
    }
  deriving Functor

data PStruct :: Type -> Type where
    PBranch   :: Map String (PStruct a)     -- ^ more components
              -> Maybe (Day Arg PStruct a)  -- ^ capture
              -> Map HTTP.Method (Ap Opt a) -- ^ endpoints
              -> PStruct a
  deriving Functor

makeBaseFunctor ''PStruct

structParser :: PStruct a -> Parser a
structParser = cata go
  where
    go :: forall x. PStructF x (Parser x) -> Parser x
    go = \case
      PBranchF cs c eps ->
        let (Any anySubs, subs) = M.foldMapWithKey mkCmd $ cs
            subp
              | anySubs   = subparser subs
              | otherwise = empty
            cap  = maybe empty mkArg c
            epMap = runAp mkOpt <$> eps
            ep = case M.minView epMap of
              Nothing -> empty
              Just (m0, ms)
                | M.null ms -> m0
                | otherwise -> subparser $ M.foldMapWithKey pickMethod epMap
        in  subp <|> cap <|> ep
    mkCmd :: String -> Parser x -> (Any, Mod CommandFields x)
    mkCmd c p = (Any True, command c $ info (p <**> helper) mempty)
    mkArg :: Day Arg PStruct x -> Parser x
    mkArg (Day Arg{..} p f) = f <$> argument argRead mods
                                <*> structParser p
      where
        mods = help argDesc
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
    pickMethod :: BS.ByteString -> Parser x -> Mod CommandFields x
    pickMethod m p = command (T.unpack . T.decodeUtf8 $ m) $ info (p <**> helper) mempty

altPStruct :: PStruct a -> PStruct a -> PStruct a
altPStruct (PBranch cs1 c1 ep1) (PBranch cs2 c2 ep2) = PBranch cs3 c3 ep3
  where
    cs3 = case c1 of
      Just _  -> cs1
      Nothing -> M.unionWith altPStruct cs1 cs2
    c3  = c1 <|> c2
    ep3 = ep1 <> ep2

instance Semigroup (PStruct a) where
    (<>) = altPStruct

instance Monoid (PStruct a) where
    mempty = PBranch M.empty Nothing M.empty

branch :: PStruct a -> PStruct b -> PStruct (Either a b)
branch x y = (Left <$> x) `altPStruct` (Right <$> y)

infixr 3 `branch`

($:>) :: String -> PStruct a -> PStruct a
c $:> p = PBranch (M.singleton c p) Nothing M.empty

infixr 4 $:>

(?:>) :: forall a b. Opt a -> PStruct (a -> b) -> PStruct b
o ?:> PBranch cs c ep = PBranch cs' c' ep'
  where
    cs' = (o ?:>) <$> cs
    c'  = c <&> \case
        Day a p f -> let f' x y z = f z x y
                     in  Day a (o ?:> (f' <$> p)) (&)
    ep' = (<*> liftAp o) <$> ep

infixr 4 ?:>

(#:>) :: Arg a -> PStruct (a -> b) -> PStruct b
a #:> p = PBranch M.empty (Just (Day a p (&))) M.empty

infixr 4 #:>

endpoint :: HTTP.Method -> a -> PStruct a
endpoint m = PBranch M.empty Nothing . M.singleton m . pure

orRequired :: ReadM a -> Coyoneda OptRead a
orRequired = liftCoyoneda . ORRequired

orOptional :: ReadM a -> Coyoneda OptRead (Maybe a)
orOptional = liftCoyoneda . OROptional

orSwitch :: Coyoneda OptRead Bool
orSwitch = liftCoyoneda ORSwitch

testStruct = hello
  where
    hello = "hello"
        $:> Arg "name" "<name>" (str @String)
        #:> Opt "capital" "capital" "BOOL" orSwitch
        ?:> endpoint HTTP.methodGet (,)

-- data Arg a = Arg
--     { argDesc :: String
--     , argMeta :: String
--     , argRead :: ReadM a
--     }
--   deriving Functor


-- -- | Query parameters are interpreted as Opt
-- data Opt a = Opt
--     { optName :: String
--     , optDesc :: String
--     , optMeta :: String
--     , optRead :: Coyoneda OptRead a
--     }
--   deriving Functor


-- -- API specification
-- type TestApi =
--        -- GET /hello/:name?capital={true, false}  returns a Greet as JSON or PlainText
--        "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON, PlainText] Greet

--        -- POST /greet with a Greet as JSON in the request body,
--        --             returns a Greet as JSON
--   :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] (Headers '[Header "X-Example" Int] Greet)

--        -- DELETE /greet/:greetid
--   :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent

-- altPStruct :: PStruct a -> PStruct a -> PStruct a
-- altPStruct x y = either id id <$> branch x y

        -- \case
    -- PBranch cs1 (Just c1) ep1 -> \case
      -- PBranch _ _ ep2 -> PBranch (fmap Left <$> cs1) (Just (Left <$> c1))
        --                          ((fmap Left <$> ep1) <> (fmap Right <$> ep2))
    -- PBranch cs1 Nothing ep1 -> \case
      -- PBranch cs2 c2 ep2 -> \case


-- testParser :: PStruct (Either Int (Either Bool String))
-- testParser = (`PBranch` Nothing) . M.fromList $
--     [ ("foo", fmap Left  . PEndpoint . liftAp $ Opt "o1" "opt 1" "INT" (auto @Int))
--     , ("bar", fmap Right . (`PBranch` Nothing) . M.fromList $
--         [ ("path1", fmap Left  . PEndpoint . liftAp $ Opt "o2" "opt 2" "BOOL" (auto @Bool))
--         , ("path2", Right <$> PEndpoint (pure "ok"))
--         ]
--       )
--     , ("baz", fmap (Right . Right) . PBranch M.empty . Just . Comp1 $
--           Coyoneda (PEndpoint . pure) (Opt "o3" "opt 3" "STRING" (liftYoneda (ORRequired (str @String))))
--       )
--     ]

-- structParser :: PStruct a -> Parser a
-- structParser = \case
--     PBranch m -> foldMap
-- structParser = \case
--     Free (Comp1 coms) -> subparser . foldMap mkCmd $ coms
--     FM.Pure x         -> pure x
--   where
--     mkCmd :: (Ap Opt :*: Com) (PStruct a) -> Mod CommandFields a
--     mkCmd (os :*: Com{..}) = command comName $
--         info (BindP (runAp mkOpt os <**> helper) structParser) (progDesc comDesc)
--     mkOpt :: Opt x -> Parser x
--     mkOpt Opt{..} = option optRead
--         ( long optName
--        <> help optDesc
--        <> metavar optMeta
--         )


-- type PStruct = Free ([] :.: (Ap Opt :*: Com))

-- structParser :: PStruct a -> Parser a
-- structParser = \case
--     Free (Comp1 coms) -> subparser . foldMap mkCmd $ coms
--     FM.Pure x         -> pure x
--   where
--     mkCmd :: (Ap Opt :*: Com) (PStruct a) -> Mod CommandFields a
--     mkCmd (os :*: Com{..}) = command comName $
--         info (BindP (runAp mkOpt os <**> helper) structParser) (progDesc comDesc)
--     mkOpt :: Opt x -> Parser x
--     mkOpt Opt{..} = option optRead
--         ( long optName
--        <> help optDesc
--        <> metavar optMeta
--         )

-- testParser :: PStruct (Either Int (Either Bool String))
-- testParser = Free . Comp1 $
--     [ fmap Left <$> liftAp (Opt "o1" "opt 1" "INT" (pure <$> auto @Int)) :*: Com "foo" "an int"
--     , fmap Right <$> pure innerTest :*: Com "bar" "something"
--     ]
--   where
--     innerTest :: PStruct (Either Bool String)
--     innerTest = liftF . Comp1 $
--       [ Left <$> liftAp (Opt "o2" "opt 2" "BOOL" auto) :*: Com "path1" "path 1"
--       -- , Right <$> liftAp (Opt "o3" "opt 3" "STRING" str) :*: Com "path2" "path 2"
--       , Right <$> pure "ok" :*: Com "path2" "path 2"
--       ]

-- testParser = liftF . Comp1 $
--     [ Left  <$> (liftAp (Opt "o1" "opt 1" "INT"  auto) :*: Com "foo" "an int")
--     , Right . Left <$> (liftAp (Opt "o2" "opt 2" "BOOL" auto) :*: Com "bar" "a bool")
--     , _
--     -- , Right . Right <$> (liftAp (Opt "o3" "opt 3" "STRING" str) :*: Com "bar" "a bool")
--     ]

-- type Com = Alt ComF

-- comParser
--     :: Alt Com a
--     -> Parser a
-- comParser = subparser . foldMap go . alternatives
--   where
--     go :: AltF Com x -> Mod CommandFields x
--     go = \case
--       Alt.Ap Com{..} y -> _ x y
--       Alt.Pure _ -> mempty



-- -- | Assorted information for making a command line argument or option
-- data PInfo a = PInfo
--     { piName :: String
--     , piDesc :: String
--     , piMeta :: String
--     , piRead :: ReadM a
--     }

-- type Opt = PInfo
-- type Arg = P

-- data StructureLayer :: Type -> Type where
--     SL :: Alt PInfo (x -> a)   -- ^ options
--        -> PInfo x              -- ^ command
--        -> StructureLayer a

--     SLOpt (Alt PInfo)


-- type Structure = Free (Ap PInfo :+: PInfo)

-- mkParser :: Structure a -> Parser a
-- mkParser = fromM . foldFree (oneM . go)
--   where
--     go :: (Ap PInfo :+: PInfo) x -> Parser x
--     go (L1 os         ) = runAp mkOpt os
--     go (R1 (PInfo{..})) = subparser $
--         command piName undefined
--     mkOpt :: PInfo x -> Parser x
--     mkOpt PInfo{..} = option piRead
--         ( long piName
--        <> help piDesc
--        <> metavar piMeta
--         )

-- toParser
--     :: Coyoneda (Ap Opt) a
--     -> Parser a
-- toParser (Coyoneda f os) = f <$> runAp go os
--   where
--     go :: Opt x -> Parser x
--     go Opt{..} = option optRead
--         ( long optName
--        <> help optDesc
--        <> metavar optMeta
--         )


-- f a = Demand { demandParser :: Parser a
--                          , demandNext   :: a -> b
--                          }

-- data Opts :: [Type] -> Type where
--     OBranch :: Rec NextStep as
--             -> Opts as

-- parserFromRouter :: Router' env a -> Parser a
-- parserFromRouter = \case
--     StaticRouter _ _ -> undefined

-- data Component = CStatic
--                | CCapture

-- data Endpoint :: [(Component, Symbol)]

-- data Route :: Type -> Type where

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
