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
    -- parseClient
    HasCLI, CLIParam, CLIResult, CLIHandler, cliHandler
  -- * Lower-level
  -- , clientPStruct
  -- , structParser
  -- * Re-export
  , ParseBody(..), defaultParseBody
  , ToCapture(..), DocCapture(..)
  , ToParam(..), DocQueryParam(..)
  ) where

import           Data.Proxy
import           Options.Applicative
import           Servant.CLI.HasCLI
import           Servant.CLI.ParseBody
import           Servant.CLI.Structure
import           Servant.Client.Core
import           Servant.Docs
import           Data.Vinyl

    
---- | Create a structure for a command line parser.
--clientPStruct
--    :: HasCLI m api
--    => Proxy api                -- ^ API
--    -> Proxy m                  -- ^ Client monad
--    -> PStruct (CLI m api)
--clientPStruct pa pm = ($ defaultRequest) <$> clientPStruct_ pm pa

-- | Create a structure for a command line parser.
--
-- If the no pa
cliPStruct
    :: HasCLI m api
    => Proxy m
    -> Proxy api
    -> HList (CLIParam m api)
    -> PStruct (m (CLIResult m api))
cliPStruct pm pa = fmap ($ defaultRequest) . cliPStruct_ pm pa


---- | Parse a servant client; the result can be run.  A good choice of @m@
---- is 'Servant.Client.ClientM'.
----
---- It takes options on how the top-level prompt is displayed when given
---- @"--help"@; it can be useful for adding a header or program description.
---- Otherwise, just use 'mempty'.
--parseClient
--    :: HasCLI m api
--    => Proxy api                -- ^ API
--    -> Proxy m                  -- ^ Client monad
--    -> InfoMod (CLI m api)      -- ^ Options for top-level display
--    -> IO (CLI m api)
--parseClient pa pm im = execParser . flip structParser im $ clientPStruct pa pm
