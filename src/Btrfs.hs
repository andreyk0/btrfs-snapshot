{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Btrfs
  ( module Btrfs.FilePath
  , module Btrfs.Types
  , btrfsSubvolSnapshot
  , btrfsSubvolTransfer
  ) where


import           Btrfs.FilePath
import           Btrfs.Types
import qualified Data.List      as List
import           Data.Time
import           Import
import           RIO.Directory
import           RIO.FilePath
import           RIO.Process
import           Types


btrfsSubvolSnapshot
  :: HasCLI env
  => BtrfsFilePath
  -> RIO env ()
btrfsSubvolSnapshot fPathFrom = do
  sRoot <- btrfsSnapshotRoot fPathFrom
  ts <- liftIO $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) <$> getCurrentTime
  fPathToRoot <-
    case List.stripPrefix (bfpMountPoint fPathFrom) (bfpCanonicalFilePath fPathFrom)
      of Nothing -> throwM $ BtrfsException $ "Error stripping mount point from " <> tshow fPathFrom
         Just relPath -> pure $ addTrailingPathSeparator (bfpCanonicalFilePath sRoot) <> relPath

  let fPathTo = addTrailingPathSeparator fPathToRoot <> ts
  createDirectoryIfMissing True fPathToRoot

  proc "btrfs" ["subvolume", "snapshot", "-r", bfpSubvol fPathFrom, fPathTo] runProcess_


-- | Subvolume transfer (full/incremental)
btrfsSubvolTransfer
  :: HasCLI env
  => Maybe FilePath -- ^ incremental if defined
  -> FilePath -- ^ from path
  -> FilePath -- ^ to path (parent directory)
  -> RIO env ()
btrfsSubvolTransfer fParent fFrom fTo = do
  let parentParams = maybe [] (\p -> ["-p", p]) fParent
      btrfsSend = proc "btrfs" $ ["send"] <> parentParams <> [fFrom]
      btrfsRecv = proc "btrfs" ["receive", fTo]

  sendProc <- btrfsSend (startProcess . setStdout createPipe)
  btrfsRecv (runProcess_ . setStdin (useHandleClose (getStdout sendProc)))
  checkExitCode sendProc
