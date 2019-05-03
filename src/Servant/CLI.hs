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

-- |
-- Module      : Servant.CLI
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Parse command line arguments into a servant client, from a servant API.
--
-- Mainly used through 'parseClient' and 'parseHandleClient'.
-- 'parseClient' returns a servant client action that returns nested
-- 'Either's for every endpoint, but 'parseHandleClient' allows you to
-- conveniently specify how you want to sort each endpoint entry into
-- a single result.
--
-- See <https://hackage.haskell.org/package/servant-cli README> for
-- a tutorial.
module Servant.CLI (
  -- * Parse Client
    parseClient, parseHandleClient
  -- ** With context
  , parseClientWithContext, parseHandleClientWithContext
  -- * Typeclasses
  , HasCLI (CLIResult, CLIHandler, cliHandler)
  -- * Context
  , ContextFor(..)
  , NamedContext(..)
  , descendIntoNamedContext
  -- * Lower-level
  , cliPStruct, cliPStructWithContext
  , structParser
  -- ** With context
  , cliHandlePStruct, cliHandlePStructWithContext
  -- * Re-export
  , ParseBody(..), defaultParseBody
  , ToCapture(..), DocCapture(..)
  , ToParam(..), DocQueryParam(..), ParamKind(..)
  , ToAuthInfo(..), DocAuthentication(..)
  ) where

import           Data.Proxy
import           Data.Vinyl
import           Options.Applicative
import           Servant.CLI.HasCLI
import           Servant.CLI.Internal.PStruct
import           Servant.CLI.ParseBody
import           Servant.Client.Core
import           Servant.Docs.Internal

-- | A version of 'cliPStruct' that can be used if the API requires
-- any external context to generate runtime data.
cliPStructWithContext
    :: HasCLI m api context
    => Proxy m                          -- ^ Client monad
    -> Proxy api                        -- ^ API
    -> Rec (ContextFor m) context       -- ^ Extra context
    -> PStruct (m (CLIResult m api))
cliPStructWithContext pm pa = fmap ($ defaultRequest)
                            . cliPStructWithContext_ pm pa

-- | A version of 'cliHandlePStruct' that can be used if the API requires
-- any external context to generate runtime data.
cliHandlePStructWithContext
    :: forall m api context r. (HasCLI m api context, Functor m)
    => Proxy m                          -- ^ Client monad
    -> Proxy api                        -- ^ API
    -> Rec (ContextFor m) context       -- ^ Extra context
    -> CLIHandler m api r               -- ^ Handler
    -> PStruct (m r)
cliHandlePStructWithContext pm pa p h =
       fmap (cliHandler pm pa (Proxy @context) h)
   <$> cliPStructWithContext pm pa p

-- | A version of 'parseClient' that can be used if the API requires
-- any external context to generate runtime data.
parseClientWithContext
    :: HasCLI m api context
    => Proxy api                        -- ^ API
    -> Proxy m                          -- ^ Client monad
    -> Rec (ContextFor m) context       -- ^ Extra context
    -> InfoMod (m (CLIResult m api))    -- ^ Options for top-level display
    -> IO (m (CLIResult m api))
parseClientWithContext pa pm p im = execParser . flip structParser im
                                  $ cliPStructWithContext pm pa p

-- | A version of 'parseHandleClient' that can be used if the API requires
-- any external context to generate runtime data.
parseHandleClientWithContext
    :: forall m api context r. (HasCLI m api context, Functor m)
    => Proxy api                        -- ^ API
    -> Proxy m                          -- ^ Client monad
    -> Rec (ContextFor m) context       -- ^ Extra context
    -> InfoMod (m (CLIResult m api))    -- ^ Options for top-level display
    -> CLIHandler m api r               -- ^ Handler
    -> IO (m r)
parseHandleClientWithContext pa pm p im h =
       fmap (cliHandler pm pa (Proxy @context) h)
   <$> parseClientWithContext pa pm p im

-- | Create a structure for a command line parser.
--
-- This can be useful if you are combining functionality with existing
-- /optparse-applicative/ parsers.  You can convert a 'PStruct' to
-- a 'Parser' using 'structParser'.
cliPStruct
    :: HasCLI m api '[]
    => Proxy m                          -- ^ Client monad
    -> Proxy api                        -- ^ API
    -> PStruct (m (CLIResult m api))
cliPStruct pm pa = cliPStructWithContext pm pa RNil

-- | Create a structure for a command line parser, producing results
-- according to a 'CLIHandler'.  See 'parseHandleClient' for more
-- information.
--
-- This can be useful if you are combining functionality with existing
-- /optparse-applicative/ parsers.  You can convert a 'PStruct' to
-- a 'Parser' using 'structParser'.
cliHandlePStruct
    :: (HasCLI m api '[], Functor m)
    => Proxy m                          -- ^ Client monad
    -> Proxy api                        -- ^ API
    -> CLIHandler m api r               -- ^ Handler
    -> PStruct (m r)
cliHandlePStruct pm pa = cliHandlePStructWithContext pm pa RNil

-- | Parse a servant client; the result can be run.  The choice of @m@
-- gives the backend you are using; for example, the default GHC
-- /servant-client/ backend is 'Servant.Client.ClientM'.
--
-- Returns the request response, which is usually a layer of 'Either' for
-- every endpoint branch.  You can find the response type directly by using
-- typed holes or asking ghci with @:t@ or @:kind! forall m. CLIResult
-- m MyAPI@.  Because it might be tedious handling nested 'Either's, see
-- 'parseHandleClient' for a way to handle each potential branch in
-- a convenient way.
--
-- Takes options on how the top-level prompt is displayed when given
-- @"--help"@; it can be useful for adding a header or program description.
-- Otherwise, just use 'mempty'.
parseClient
    :: HasCLI m api '[]
    => Proxy api                        -- ^ API
    -> Proxy m                          -- ^ Client monad
    -> InfoMod (m (CLIResult m api))    -- ^ Options for top-level display
    -> IO (m (CLIResult m api))
parseClient pa pm = parseClientWithContext pa pm RNil

-- | Parse a server client, like 'parseClient'.  However, instead of that
-- client action returning the request response, instead use a 'CLIHandler'
-- to handle every potential request response.  It essentially lets you
-- specify how to sort each potential endpoint's response into a single
-- output value.
--
-- The handler is usually a 'Servant.API.:<|>' for every endpoint branch.
-- You can find it by using typed holes or asking ghci with @:t@ or @:kind!
-- forall m r.  CLIHandler m MyAPI r@.
--
-- Takes options on how the top-level prompt is displayed when given
-- @"--help"@; it can be useful for adding a header or program description.
-- Otherwise, just use 'mempty'.
parseHandleClient
    :: (HasCLI m api '[], Functor m)
    => Proxy api                        -- ^ API
    -> Proxy m                          -- ^ Client monad
    -> InfoMod (m (CLIResult m api))    -- ^ Options for top-level display
    -> CLIHandler m api r               -- ^ Handler
    -> IO (m r)
parseHandleClient pa pm = parseHandleClientWithContext pa pm RNil

