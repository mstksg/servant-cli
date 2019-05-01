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
    HasCLI(..)
  , clientPStruct
  , parseClient
  , ParseBody(..)
  , defaultParseBody
  , structParser
  -- * Re-export
  , ToSample(..)
  , ToCapture(..), DocCapture(..)
  , ToParam(..), DocQueryParam(..)
  ) where

import           Data.Proxy
import           Options.Applicative
import           Servant.CLI.Internal
import           Servant.CLI.Structure
import           Servant.Client.Core
import           Servant.Docs

-- | Create a structure for a command line parser.
clientPStruct
    :: HasCLI m api
    => Proxy api
    -> Proxy m
    -> PStruct (CLI m api)
clientPStruct pa pm = ($ defaultRequest) <$> clientParser_ pm pa

-- | Parse a servant client; the result can be run.  A good choice of @m@
-- is 'ClientM'.
--
-- It takes options on how the top-level prompt is displayed when given
-- @"--help"@; it can be useful for adding a header or program description.
-- Otherwise, just use 'mempty'.
parseClient
    :: HasCLI m api
    => Proxy api
    -> Proxy m
    -> InfoMod (CLI m api)
    -> IO (CLI m api)
parseClient pa pm im = execParser . flip structParser im $ clientPStruct pa pm
