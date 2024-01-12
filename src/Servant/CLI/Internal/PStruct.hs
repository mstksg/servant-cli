{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

-- |
-- Module      : Servant.CLI.PStruct
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal module providing a data structure for representing structure of
-- command line parsers that can be manipulated as an ADT, as well as
-- functionality to interpret it as a 'Parser' command line argument
-- parser.
module Servant.CLI.Internal.PStruct (
    OptRead(..)
  , Opt(..)
  , Arg(..)
  , MultiArg(..)
  , Captures
  , Endpoint(..)
  , EndpointMap(..)
  , PStruct(..)
  , PStructF(..)
  , structParser
  , structParser_
  -- * Creating
  , branch
  , ($:>), (%:>), (?:>), (#:>), (##:>), note, endpoint, rawEndpoint
  -- ** Readers
  , orRequired, orOptional, orSwitch
  ) where

import           Control.Applicative.Backwards
import           Control.Applicative.Free
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.Functor.Combinator
import           Data.Functor.Combinator.Unsafe
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.Kind
import           Data.List.NonEmpty              (NonEmpty(..))
import           Data.Map                        (Map)
import           Data.Maybe
import           Data.Proxy
import           Options.Applicative
import           System.FilePath
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
    , optVals :: Maybe (NonEmpty String)
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

-- | Interpret an 'Arg' as something that can be given repeatedly an
-- arbitrary number of times.
data MultiArg :: Type -> Type where
    MultiArg :: Arg a -> MultiArg [a]

-- | A map of endpoints associated with methods, paired with an optional
-- "raw" endpoint.
data EndpointMap a = EPM
    { epmGiven :: Map HTTP.Method (Endpoint a)
    , epmRaw   :: Maybe (Endpoint (HTTP.Method -> a))
    }
  deriving Functor

-- | Captures can be a single capture leading to the next level, or
-- a multi-capture leading to an endpoint action.
type Captures = Day Arg      PStruct
            :+: Day MultiArg EndpointMap

-- | Endpoint arguments and body.
newtype Endpoint a = Endpoint
    { epStruct :: Day (Ap Opt) Parser a }
  deriving Functor

-- | Structure for a parser of a given value that may use items from
-- captures and arguments.
data PStruct a = PStruct
    { psInfo       :: [String]
    , psComponents :: Map String (PStruct a)         -- ^ path components
    , psCaptures   :: Maybe (Captures a)             -- ^ captures
    , psEndpoints  :: EndpointMap a
    }
  deriving Functor
-- TODO: Capture vs. Endpoint interplay is a bit weird, when they are at
-- the same level.

makeBaseFunctor ''PStruct

-- | Convert a 'PStruct' into a command line argument parser, from the
-- /optparse-applicative/ library.  It can be run with 'execParser'.
--
-- It takes options on how the top-level prompt is displayed when given
-- @"--help"@; it can be useful for adding a header or program description.
-- Otherwise, just use 'mempty'.
structParser
    :: PStruct a        -- ^ The 'PStruct' to convert.
    -> InfoMod a        -- ^ Modify how the top-level prompt is displayed.
    -> ParserInfo a
structParser = flip $ \im -> ($ im) . ($ []) . ($ True) . structParser_

-- | Low-level implementation of 'structParser'.
structParser_
    :: PStruct a
    -> Bool         -- ^ add helper
    -> [String]     -- ^ root path
    -> InfoMod a    -- ^ modify top level
    -> ParserInfo a
structParser_ = cata go
  where
    go  :: PStructF x (Bool -> [String] -> InfoMod x -> ParserInfo x)
        -> Bool
        -> [String]
        -> InfoMod x
        -> ParserInfo x
    go PStructF{..} toHelp p im = info ((subp <|> cap <|> ep) <**> mkHelp) $
           fullDesc
        <> header (joinPath p)
        <> progDescDoc (Just (O.vcat . map O.pretty $ ns))
        <> im
      where
        subs = M.foldMapWithKey (mkCmd p) psComponentsF
        subp
          | M.null psComponentsF = empty
          | otherwise            = subparser $ subs
                                            <> metavar "COMPONENT"
                                            <> commandGroup "Path components:"
        cap  = unsafePlus (Proxy @Parser) $
                  interpret (mkArg p !*! mkArgs) $ MaybeF psCapturesF
        ep   = methodPicker psEndpointsF
        ns   = psInfoF
        mkHelp
          | toHelp    = helper
          | otherwise = pure id
    mkCmd
        :: [String]
        -> String
        -> (Bool -> [String] -> InfoMod x -> ParserInfo x)
        -> Mod CommandFields x
    mkCmd ps c p = command c (p True (ps ++ [c]) mempty)
    mkArg :: [String] -> Day Arg PStruct x -> Parser x
    mkArg ps (Day a p f) =
          f <$> argParser a
            <*> infoParser (structParser_ p False (ps ++ [':' : argName a]) mempty)
    mkArgs :: Day MultiArg EndpointMap x -> Parser x
    mkArgs = unsafeApply (Proxy @Parser) $
           forwards
         . ( Backwards . (\case MultiArg a -> many (argParser a))
         !*! Backwards . methodPicker
           )
    argParser :: Arg x -> Parser x
    argParser Arg{..} = argument argRead $ help argDesc
                                        <> metavar argMeta
    mkOpt :: Opt x -> Parser x
    mkOpt Opt{..} = forI optRead $ \case
        ORRequired r -> option r mods
        OROptional r -> optional $ option r mods
        ORSwitch     -> switch   $ long optName <> help optDesc
      where
        mods :: Mod OptionFields y
        mods = long optName
            <> help optDesc
            <> metavar optMeta
            <> foldMap (completeWith . toList) optVals
    methodPicker :: EndpointMap x -> Parser x
    methodPicker (EPM eps rw) = case M.minView epMap of
        Nothing       -> maybe empty mkRaw rw
        Just (m0, ms)
          | M.null ms && isNothing rw -> m0
          | otherwise -> subparser $ M.foldMapWithKey pickMethod epMap
                                  <> foldMap mkRawCommand rw
                                  <> metavar "METHOD"
                                  <> commandGroup "HTTP Methods:"
      where
        epMap = mkEndpoint <$> eps
    mkEndpoint :: Endpoint x -> Parser x
    mkEndpoint = unsafeApply (Proxy @Parser) $
        binterpret (interpret mkOpt) id
      . epStruct
    pickMethod :: HTTP.Method -> Parser x -> Mod CommandFields x
    pickMethod m p = command (T.unpack . T.decodeUtf8 $ m) $ info (p <**> helper) mempty
    mkRaw :: Endpoint (HTTP.Method -> x) -> Parser x
    mkRaw e = mkEndpoint e <*> o
      where
        o = strOption @HTTP.Method $
              long "method"
           <> help "method for raw request (GET, POST, etc.)"
           <> metavar "METHOD"
           <> completeWith (show <$> [HTTP.GET ..])
    mkRawCommand :: Endpoint (HTTP.Method -> x) -> Mod CommandFields x
    mkRawCommand d = command "RAW" $ info (mkRaw d <**> helper) mempty

-- | Combine two 'EndpointMap's, preferring the left hand side for
-- conflicts.  If the left hand has a raw endpoint, the right hand's
-- endpoints are ignored.
instance Semigroup (EndpointMap a) where
    (<>) = altEPM

instance Monoid (EndpointMap a) where
    mempty = EPM M.empty Nothing

altEPM :: EndpointMap a -> EndpointMap a -> EndpointMap a
altEPM (EPM e1 r1) (EPM e2 r2) = EPM e3 r3
  where
    e3  = case r1 of
      Just _  -> e1
      Nothing -> M.unionWith const e1 e2
    r3  = r1 <|> r2

altPStruct :: PStruct a -> PStruct a -> PStruct a
altPStruct (PStruct ns1 cs1 c1 ep1) (PStruct ns2 cs2 c2 ep2) =
    PStruct ns3 cs3 c3 ep3
  where
    ns3 = ns1 ++ ns2    -- ??
    cs3 = case c1 of
      Just _  -> cs1
      Nothing -> M.unionWith altPStruct cs1 cs2
    c3  = c1 <|> c2
    ep3 = ep1 <> ep2

-- | Combine two 'PStruct's, preferring the left hand side for conflicts.
-- If the left hand has a capture, the right hand's components are ignored.
-- If the left hand has a raw endpoint, the right hand's endpoints are
-- ignored.
instance Semigroup (PStruct a) where
    (<>) = altPStruct

instance Monoid (PStruct a) where
    mempty = PStruct [] M.empty Nothing mempty

-- | Combine two 'PStruct's in an either-or fashion, favoring the left hand
-- side.
branch :: PStruct a -> PStruct b -> PStruct (Either a b)
branch x y = (Left <$> x) `altPStruct` (Right <$> y)

infixr 3 `branch`

-- | Shift by a path component.
($:>) :: String -> PStruct a -> PStruct a
c $:> p = mempty { psComponents = M.singleton c p }
infixr 4 $:>

-- | Add a command-line option to all endpoints.
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
          in  R1 $ Day a (addEPMOpt o (f' <$> p)) (&)
    ep' = addEPMOpt o ep
infixr 4 ?:>

addEndpointOpt :: Opt a -> Endpoint (a -> b) -> Endpoint b
addEndpointOpt o (Endpoint (Day eo eb ef)) =
    Endpoint (Day ((,) <$> inject o <*> eo) eb $ \(x, y) z -> ef y z x)

addEPMOpt :: Opt a -> EndpointMap (a -> b) -> EndpointMap b
addEPMOpt o (EPM e r) = EPM e' r'
  where
    e' = addEndpointOpt o <$> e
    r' = addEndpointOpt o . fmap flip <$> r

-- | Add notes to the beginning of a documentation level.
note :: [String] -> PStruct a -> PStruct a
note ns (PStruct ms cs c ep) = PStruct (ns ++ ms) cs c ep
infixr 4 `note`

-- | Add a single argument praser.
(#:>) :: Arg a -> PStruct (a -> b) -> PStruct b
a #:> p = mempty { psCaptures = Just (L1 (Day a p (&))) }
infixr 4 #:>

-- | Add a repeating argument parser.
(##:>) :: Arg a -> PStruct ([a] -> b) -> PStruct b
a ##:> p = mempty
    { psCaptures = Just (R1 (Day (MultiArg a) (psEndpoints p) (&)))
    }
infixr 4 ##:>

-- | Add a request body to all endpoints.
--
-- If done more than once per endpoint, it runs *both* parsers; however,
-- we can only send one request body, so this is undefined behavior as
-- a client.
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
          in  R1 $ Day a (addEPMBody b (f' <$> p)) (&)
    ep' = addEPMBody b ep
infixr 4 %:>

addEndpointBody :: Parser a -> Endpoint (a -> b) -> Endpoint b
addEndpointBody b (Endpoint d) =
    Endpoint (inR b <**> d)

addEPMBody :: Parser a -> EndpointMap (a -> b) -> EndpointMap b
addEPMBody b (EPM e r) = EPM e' r'
  where
    e' = addEndpointBody b <$> e
    r' = addEndpointBody b . fmap flip <$> r

-- | Create an endpoint action.
endpoint :: HTTP.Method -> a -> PStruct a
endpoint m x = mempty
    { psEndpoints = EPM (M.singleton m (Endpoint (pure x))) Nothing
    }

-- | Create a raw endpoint.
rawEndpoint :: (HTTP.Method -> a) -> PStruct a
rawEndpoint f = mempty
    { psEndpoints = EPM M.empty (Just (Endpoint (pure f)))
    }

-- | Helper to lift a 'ReadM' into something that can be used with 'optRead'.
orRequired :: ReadM a -> Coyoneda OptRead a
orRequired = inject . ORRequired

-- | Helper to lift an optional 'ReadM' into something that can be used
-- with 'optRead'.
orOptional :: ReadM a -> Coyoneda OptRead (Maybe a)
orOptional = inject . OROptional

-- | An 'optRead' that is on-or-off.
orSwitch :: Coyoneda OptRead Bool
orSwitch = inject ORSwitch

