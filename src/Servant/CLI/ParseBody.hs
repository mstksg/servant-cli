{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- |
-- Module      : Servant.CLI.ParseBody
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides the interface for 'ParseBody', a helper class for defining
-- directly how to parse request bodies.
module Servant.CLI.ParseBody (
    ParseBody(..)
  , defaultParseBody
  ) where

import           Data.Char
import           Options.Applicative
import           Text.Printf
import           Type.Reflection
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL

-- | A helper class for defining directly how to parse request bodies.
-- This allows more complex parsing of bodies.
--
-- You need an instance of this for every type you use with 'ReqBody'.
class ParseBody a where
    parseBody :: Parser a

    default parseBody :: (Typeable a, Read a) => Parser a
    parseBody = defaultParseBody (show (typeRep @a)) auto

-- | Default implementation that expects a @--data@ option.
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

instance ParseBody T.Text where
    parseBody = defaultParseBody "Text" str

instance ParseBody TL.Text where
    parseBody = defaultParseBody "Text" str

instance ParseBody Int where
instance ParseBody Integer where
instance ParseBody Float where
instance ParseBody Double where
