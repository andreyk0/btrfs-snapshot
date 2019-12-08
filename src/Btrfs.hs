{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}


module Btrfs
  ( BtrfsException(..)
  , btrfsMounts
  , btrfsSubvolSnapshot
  , btrfsSubvolTransfer
  , findBtrfsSubvol
  , findBtrfsSubvolSnapshots
  ) where


import qualified Data.List               as List
import qualified Data.Text.IO            as TIO
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LTE
import           Data.Time
import           Import
import           RIO.Directory
import           RIO.FilePath
import           RIO.Process
import qualified RIO.Text                as T
import           System.Process.Typed    (nullStream)
import           Types


newtype BtrfsException = BtrfsException Text deriving (Eq,Show,Generic)
instance Exception BtrfsException

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


{- | Walk the path up until we hit subvolume boundary.
     Returns canonical absolute subvolume path.
-}
findBtrfsSubvol
  :: HasCLI env
  => FilePath
  -> RIO env FilePath
findBtrfsSubvol fPath' = do
  fPath <- canonicalizePath fPath'
  isSv <- isBtrfsSubvol fPath

  if isSv
    then pure fPath
    else case (fPath, takeDirectory fPath)
           of ("/", _)     -> throwM $ BtrfsException "Failed to find btrfs subvol in /"
              (".", _)     -> throwM $ BtrfsException "Failed to get canonical path"
              (_, fParent) -> findBtrfsSubvol fParent


-- | Find btrfs subvolume snapshots matching prefix.
findBtrfsSubvolSnapshots
  :: HasCLI env
  => FilePath
  -> RIO env [FilePath]
findBtrfsSubvolSnapshots fp' = do
  fp <- canonicalizePath fp'
  fpVol <- findBtrfsSubvol fp >>= canonicalizePath
  outBytes <- proc "btrfs" ["subvolume", "list", "-o", fpVol] readProcessStdout_

  mounts <- btrfsMounts
  fpMount <- LT.pack . addTrailingPathSeparator <$> findMountPoint mounts fpVol

  logDebug . display $ tshow (("fp"::Text, fp), ("fpVol"::Text, fpVol), ("fpMount"::Text, fpMount))

  let outTxt = LTE.decodeUtf8 outBytes
      snapPaths = catMaybes $ parseLine <$> LT.lines outTxt

      parseLine l =
        let fields = LT.split (== ' ') l
        in (fpMount <>) <$> listToMaybe (drop 8 fields)

  pure $ LT.unpack <$> filter (LT.isPrefixOf (LT.pack (addTrailingPathSeparator fp))) snapPaths


btrfsSubvolSnapshot
  :: HasCLI env
  => FilePath
  -> RIO env ()
btrfsSubvolSnapshot fPathFrom' = do
  fPathFrom <- canonicalizePath fPathFrom'
  sRoot <- btrfsSnapshotRoot fPathFrom
  ts <- liftIO $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) <$> getCurrentTime
  let fPathToRoot = sRoot <> fPathFrom
      fPathTo = addTrailingPathSeparator fPathToRoot <> ts

  createDirectoryIfMissing True fPathToRoot
  proc "btrfs" ["subvolume", "snapshot", "-r", fPathFrom, fPathTo] runProcess_


btrfsSnapshotRoot
  :: HasCLI env
  => FilePath -- ^ *canonical* file path being snapshotted (to determine btrfs files ystem root from)
  -> RIO env FilePath
btrfsSnapshotRoot fp = do
  mnts <- btrfsMounts
  fpMount <- findMountPoint mnts fp
  let snapRoot = btrfsSnapshotRootName fpMount
  logDebug . display $ tshow (("fp"::Text, fp), ("fpMount"::Text, fpMount), ("snapRoot"::Text, snapRoot))

  ex <- doesDirectoryExist snapRoot
  isSnapRoot <- isBtrfsSubvol snapRoot
  case (ex, isSnapRoot)
    of (True, True)  -> pure snapRoot
       (True, False) -> throwM $ BtrfsException $ "Refusing to run, " <>
                                 T.pack snapRoot <> " exists but it's not a btrfs subvolume"
       (False, True) -> throwM $ BtrfsException "btrfsSnapshotRoot: unexpected error"
       (False, False) -> proc "btrfs" ["subvolume", "create", snapRoot] runProcess_ >> pure snapRoot


-- | Name of the snapshots dir at the root of the btrfs volume
btrfsSnapshotRootName
  :: FilePath -- ^ btrfs mount point
  -> FilePath
btrfsSnapshotRootName mp = addTrailingPathSeparator mp <> ".snapshot"


-- | Checks if a given path is a btrfs subvolume
isBtrfsSubvol
  :: HasCLI env
  => FilePath
  -> RIO env Bool
isBtrfsSubvol fPath = do
  e <- proc "btrfs" ["subvolume", "show", fPath]
       ( runProcess
       . setStdout nullStream
       . setStderr nullStream
       )
  pure $ case e
           of ExitSuccess   -> True
              ExitFailure _ -> False


-- | Finds given path's mount point
findMountPoint
  :: [FilePath] -- ^ list of mount points
  -> FilePath -- ^ *canonical* path in question
  -> RIO env FilePath
findMountPoint mpaths fp = maybe
  (throwM (BtrfsException (tshow ("Failed to find mount point"::Text, mpaths, fp))))
  pure
  (List.find (`List.isPrefixOf` fp) mpaths)


-- | Lists btrfs mount points. Sorted, longest first.
btrfsMounts
  :: RIO env [FilePath]
btrfsMounts = do
  minfo <- T.lines <$> liftIO (TIO.readFile "/proc/self/mountinfo")
  let mps = catMaybes (parseLine <$> minfo)
      mpsSorted = List.sortBy (flip compare) mps
  pure $ T.unpack <$> mpsSorted

  where
    parseLine (T.split (== ' ') -> (_:_:_:_:m:_:_:_:"btrfs":_)) = Just m
    parseLine _                                                 = Nothing
