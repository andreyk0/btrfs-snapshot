{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import           Import
import           Types

run
  :: HasCLIOptions env
  => HasLogFunc env
  => RIO env ()
run = do
  cmd <- view cliCommand
  case cmd
    of CLISnapshotCreate c -> runCLISnapshotCreate c
       CLISnapshotTransfer t -> runCLISnapshotTransfer t


runCLISnapshotCreate
  :: HasCLIOptions env
  => HasLogFunc env
  => CLISnapshotCreate
  -> RIO env ()
runCLISnapshotCreate (SnapshotCreate paths) = do
  logInfo . display $ tshow paths


runCLISnapshotTransfer
  :: HasCLIOptions env
  => HasLogFunc env
  => CLISnapshotTransfer
  -> RIO env ()
runCLISnapshotTransfer (SnapshotTransfer tPathFrom tPathTo) = do
  logInfo . display $ tshow (tPathFrom, tPathTo)
