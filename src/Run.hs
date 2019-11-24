{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import           Import
import           Types
import Btrfs

run
  :: HasCLI env
  => HasLogFunc env
  => RIO env ()
run = do
  cmd <- view cliCommand
  case cmd
    of CLISnapshotCreate c -> runCLISnapshotCreate c
       CLISnapshotTransfer t -> runCLISnapshotTransfer t


runCLISnapshotCreate
  :: HasCLI env
  => CLISnapshotCreate
  -> RIO env ()
runCLISnapshotCreate (SnapshotCreate paths) =
  traverse_ snapPath paths
  where
    snapPath fp = do
      findBtrfsSubvol fp
      logInfo . display $ tshow fp


runCLISnapshotTransfer
  :: HasCLI env
  => CLISnapshotTransfer
  -> RIO env ()
runCLISnapshotTransfer (SnapshotTransfer tPathFrom tPathTo) = do
  logInfo . display $ tshow (tPathFrom, tPathTo)
