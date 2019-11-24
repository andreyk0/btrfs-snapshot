{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}

module Types where

import RIO
import RIO.Process

data CLICommand
  = CLICommandCreate
  | CLICommandTransfer


-- | Command line arguments
data Options = Options
  { optionsVerbose :: Bool
  , optionsCommand :: CLICommand
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: ProcessContext
  , appOptions :: Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
