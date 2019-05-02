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

module Servant.CLI (
    parseClient, parseHandleClient
  , parseClient', parseHandleClient'
  -- * Typeclasses
  , HasCLI (CLIParam, CLIResult, CLIHandler, cliHandler)
  -- * Lower-level
  , cliPStruct, cliPStruct'
  , structParser
  -- * Re-export
  , ParseBody(..), defaultParseBody
  , ToCapture(..), DocCapture(..)
  , ToParam(..), DocQueryParam(..)
  , Rec(..)
  ) where

import           Data.Proxy
import           Options.Applicative
import           Servant.CLI.HasCLI
import           Servant.CLI.ParseBody
import           Servant.CLI.Structure
import           Servant.Client.Core
import           Servant.Docs
import           Data.Vinyl

    
-- | Create a structure for a command line parser.
--
-- Takes a 'Rec' of actions to generate required items.  Pass in 'RNil' if
-- no parameters are expected (that is, if @'CLIParam' m api@ is an empty
-- list).  The actions will only be run if they are needed.
cliPStruct
    :: HasCLI m api
    => Proxy m                          -- ^ Client monad
    -> Proxy api                        -- ^ API
    -> Rec m (CLIParam m api)           -- ^ Extra parameters
    -> PStruct (m (CLIResult m api))
cliPStruct pm pa = fmap ($ defaultRequest) . cliPStruct_ pm pa

-- | Parse a servant client; the result can be run.  A good choice of @m@
-- is 'Servant.Client.ClientM'.
--
-- Returns the request response, which is usually a layer of 'Either' for
-- every endpoint branch.  You can find the response type directly by using
-- typed holes or asking ghci with @:t@ or @:kind! forall m. CLIResult
-- m MyAPI@.  Because it might be tedious handling nested 'Either's, see
-- 'parseHandleClient' for a way to handle each potential branch in
-- a convenient way.
--
-- Takes a 'Rec' of actions to generate required items.  Pass in 'RNil' if
-- no parameters are expected (that is, if @'CLIParam' m api@ is an empty
-- list).  The actions will only be run if they are needed.
--
-- Takes options on how the top-level prompt is displayed when given
-- @"--help"@; it can be useful for adding a header or program description.
-- Otherwise, just use 'mempty'.
parseClient
    :: HasCLI m api
    => Proxy api                        -- ^ API
    -> Proxy m                          -- ^ Client monad
    -> Rec m (CLIParam m api)           -- ^ Extra parameters
    -> InfoMod (m (CLIResult m api))    -- ^ Options for top-level display
    -> IO (m (CLIResult m api))
parseClient pa pm p im = execParser . flip structParser im $ cliPStruct pm pa p

-- | Parse a server client, like 'parseClient'.  However, instead of that
-- client action returning the request response, instead use a 'CLIHandler'
-- to handle every potential request response.
--
-- The handler is usually a ':<|>' for every endpoint branch.  You can find
-- it by using typed holes or asking ghci with @:t@ or @:kind! forall m r.
-- CLIHandler m MyAPI r@.
--
-- Takes a 'Rec' of actions to generate required items.  Pass in 'RNil' if
-- no parameters are expected (that is, if @'CLIParam' m api@ is an empty
-- list).  The actions will only be run if they are needed.
--
-- Takes options on how the top-level prompt is displayed when given
-- @"--help"@; it can be useful for adding a header or program description.
-- Otherwise, just use 'mempty'.
parseHandleClient
    :: (HasCLI m api, Functor m)
    => Proxy api                        -- ^ API
    -> Proxy m                          -- ^ Client monad
    -> Rec m (CLIParam m api)           -- ^ Extra parameters
    -> InfoMod (m (CLIResult m api))    -- ^ Options for top-level display
    -> CLIHandler m api r               -- ^ Handler
    -> IO (m r)
parseHandleClient pa pm p im h =
    fmap (cliHandler pm pa h) <$> parseClient pa pm p im

-- | A version of 'cliPStruct' that works when the client action requires
-- no extra parameters.
cliPStruct'
    :: (HasCLI m api, CLIParam m api ~ '[])
    => Proxy m                          -- ^ Client monad
    -> Proxy api                        -- ^ API
    -> PStruct (m (CLIResult m api))
cliPStruct' pm pa = cliPStruct pm pa RNil

-- | A version of 'parseClient' that works when the client action requires
-- no extra parameters.
parseClient'
    :: (HasCLI m api, CLIParam m api ~ '[])
    => Proxy api                        -- ^ API
    -> Proxy m                          -- ^ Client monad
    -> InfoMod (m (CLIResult m api))    -- ^ Options for top-level display
    -> IO (m (CLIResult m api))
parseClient' pa pm = parseClient pa pm RNil

-- | A version of 'parseHandleClient' that works when the client action requires
parseHandleClient'
    :: (HasCLI m api, Functor m, CLIParam m api ~ '[])
    => Proxy api                        -- ^ API
    -> Proxy m                          -- ^ Client monad
    -> InfoMod (m (CLIResult m api))    -- ^ Options for top-level display
    -> CLIHandler m api r               -- ^ Handler
    -> IO (m r)
parseHandleClient' pa pm = parseHandleClient pa pm RNil

