{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}


module Types where

import           Import
import           RIO.Process


newtype CLISnapshotCreate = SnapshotCreate
  { _snapshotCreatePath :: [FilePath]
  } deriving (Eq,Show,Generic)

makeLenses ''CLISnapshotCreate


data CLISnapshotTransfer = SnapshotTransfer
  { _snapshotTransferFrom :: FilePath
  , _snapshotTransferTo   :: FilePath
  } deriving (Eq,Show,Generic)

makeLenses ''CLISnapshotTransfer


data CLICommand
  = CLISnapshotCreate CLISnapshotCreate
  | CLISnapshotTransfer CLISnapshotTransfer
  deriving (Eq,Show,Generic)

makeLenses ''CLICommand


-- | Command line arguments
newtype Options = Options
  { _optionsVerbose :: Bool
  }

makeLenses ''Options


data App = App
  { _appLogFunc        :: LogFunc
  , _appProcessContext :: ProcessContext
  , _appOptions        :: Options
  , _appCommand        :: CLICommand
  }

makeLenses ''App


instance HasLogFunc App where
  logFuncL = appLogFunc


instance HasProcessContext App where
  processContextL = appProcessContext


class ( HasLogFunc a
      , HasProcessContext a
      ) => HasCLI a where

  cliIsVerbose :: Lens' a Bool
  cliCommand :: Lens' a CLICommand


instance HasCLI App where
  cliIsVerbose = appOptions . optionsVerbose
  cliCommand = appCommand


