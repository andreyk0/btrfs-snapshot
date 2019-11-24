{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import           Btrfs
import           Import
import           System.Posix.User
import           Types

run
  :: HasCLI env
  => RIO env ()
run = do
  euid <- liftIO getEffectiveUserID

  unless (0 == euid) $ do
    logError "Please re-run with 'sudo'"
    exitFailure

  cmd <- view cliCommand
  case cmd
    of CLISnapshotCreate c   -> runCLISnapshotCreate c
       CLISnapshotTransfer t -> runCLISnapshotTransfer t


runCLISnapshotCreate
  :: HasCLI env
  => CLISnapshotCreate
  -> RIO env ()
runCLISnapshotCreate (SnapshotCreate paths) =
  traverse_ snapPath paths
  where
    snapPath fp = do
      sVol <- findBtrfsSubvol fp
      btrfsSubvolSnapshot sVol


runCLISnapshotTransfer
  :: HasCLI env
  => CLISnapshotTransfer
  -> RIO env ()
runCLISnapshotTransfer (SnapshotTransfer tPathFrom tPathTo) = do
  logInfo . display $ tshow (tPathFrom, tPathTo)
