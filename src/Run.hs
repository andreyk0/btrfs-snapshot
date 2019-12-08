{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import           Btrfs
import qualified Data.List         as List
import           Import
import           RIO.Directory
import           RIO.FilePath
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
      bfp <- btrfsFilePath fp
      logDebug . display $ tshow bfp
      btrfsSubvolSnapshot bfp


runCLISnapshotTransfer
  :: HasCLI env
  => CLISnapshotTransfer
  -> RIO env ()
runCLISnapshotTransfer (SnapshotTransfer tPathFrom' tPathTo') = do
  tPathFrom <- btrfsFilePath tPathFrom'
  tPathTo <- btrfsFilePath tPathTo'
  snaps <- findBtrfsSubvolSnapshots tPathFrom

  logDebug . display $ "Found snapshots: " <> tshow snaps

  snapSources <- sequence
        [ case List.stripPrefix (addTrailingPathSeparator (bfpCanonicalFilePath tPathFrom)) s
            of Nothing -> throwM . BtrfsException $ "failed to strip prefix " <> tshow (s, tPathFrom)
               Just p  -> pure (s, p)
        | s <- snaps
        ]

  let transferFromTo = List.sort
        [ (s, addTrailingPathSeparator (bfpCanonicalFilePath tPathTo) <> p)
        | (s, p) <- snapSources
        ]

      previousSnapshots = Nothing : (Just . fst <$> transferFromTo)

      incrementalTransfers = zip previousSnapshots transferFromTo <&>
        (\case (Nothing, (f,t)) -> (Nothing, f, t)
               (Just prev, (f, t)) ->
                 if takeDirectory prev == takeDirectory f
                 then (Just prev, f, t)
                 else (Nothing, f, t)
        )

  logDebug . display $ "Input: " <> tshow (tPathFrom, tPathTo, snaps)

  forM_ incrementalTransfers $ \(prev, f, t) -> do
    let tDir = takeDirectory t

    unlessM (doesDirectoryExist tDir) $ do
      logInfo . display $ "Creating " <> tshow tDir
      createDirectoryIfMissing True tDir

    unlessM (doesDirectoryExist t) $ do
      logInfo . display $ "Transferring " <> tshow (f,tDir)
      btrfsSubvolTransfer prev f tDir
