{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Servant.CLI.Structure (
    OptRead(..)
  , Opt(..)
  , Arg(..)
  , MultiArg(..)
  , Captures
  , Endpoint(..)
  , PStruct(..)
  , PStructF(..)
  , structParser
  , structParser_
  , altPStruct
  , branch
  , ($:>), (%:>), (?:>), (#:>), (##:>), note, endpoint
  , orRequired, orOptional, orSwitch
  ) where

import           Control.Applicative.Free
import           Data.Function
import           Data.Functor
import           Data.Functor.Coyoneda
import           Data.Functor.Day
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.Kind
import           Data.Map                        (Map)
import           Data.Semigroup hiding           (Option(..), option, Arg(..))
import           GHC.Generics
import           Options.Applicative
import           System.FilePath
import qualified Data.ByteString                 as BS
import qualified Data.Map                        as M
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Network.HTTP.Types              as HTTP
import qualified Options.Applicative.Help.Pretty as O


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

