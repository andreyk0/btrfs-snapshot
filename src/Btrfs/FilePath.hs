{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}


module Btrfs.FilePath
  ( btrfsFilePath
  , btrfsSnapshotRoot
  , findBtrfsSubvolSnapshots
  ) where


import           Btrfs.Types
import qualified Data.List               as List
import qualified Data.Text.IO            as TIO
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LTE
import           Import
import           RIO.Directory
import           RIO.FilePath
import           RIO.Process
import qualified RIO.Text                as T
import           System.Process.Typed    (nullStream)
import           Types


-- | Construct BtrfsFilePath from a given FilePath
btrfsFilePath
  :: HasCLI env
  => FilePath
  -> RIO env BtrfsFilePath
btrfsFilePath bfpFilePath = do
  bfpCanonicalFilePath <- canonicalizePath bfpFilePath
  bfpSubvol <- findBtrfsSubvol bfpCanonicalFilePath

  mps <- btrfsMounts
  bfpMountPoint <- findMountPoint mps bfpCanonicalFilePath

  pure $ BtrfsFilePath{..}


-- | Snapshot root directory for the given file path to be snapshotted
btrfsSnapshotRoot
  :: HasCLI env
  => BtrfsFilePath -- ^ file path to be snapshotted
  -> RIO env BtrfsFilePath
btrfsSnapshotRoot fp = do
  let snapRootName = btrfsSnapshotRootName (bfpMountPoint fp)
  snapRoot <- btrfsFilePath snapRootName
  logDebug . display $ tshow (("fp"::Text, fp), ("snapRoot"::Text, snapRoot))

  ex <- doesDirectoryExist (bfpCanonicalFilePath snapRoot)
  let isSnapRoot = bfpSubvol snapRoot == bfpCanonicalFilePath snapRoot
  case (ex, isSnapRoot)
    of (True, True)  -> pure snapRoot
       (True, False) -> throwM $ BtrfsException $ "Refusing to run, " <>
                                   tshow snapRoot <> " path exists but it's not a btrfs subvolume"
       (False, True) -> throwM $ BtrfsException "btrfsSnapshotRoot: unexpected error"
       (False, False) -> proc "btrfs" ["subvolume", "create", bfpCanonicalFilePath snapRoot]
                           runProcess_ >> pure snapRoot


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
  => BtrfsFilePath
  -> RIO env [FilePath]
findBtrfsSubvolSnapshots fp = do
  outBytes <- proc "btrfs" ["subvolume", "list", "-o", bfpSubvol fp] readProcessStdout_

  let fpMountTxt = LT.pack $ addTrailingPathSeparator $ bfpMountPoint fp
      fpTxt = LT.pack $ addTrailingPathSeparator $ bfpCanonicalFilePath fp

  logDebug . display $ tshow ("fp"::Text, fp)

  let outTxt = LTE.decodeUtf8 outBytes
      snapPaths = catMaybes $ parseLine <$> LT.lines outTxt

      parseLine l =
        let fields = LT.split (== ' ') l
        in (fpMountTxt <>) <$> listToMaybe (drop 8 fields)

  pure $ LT.unpack <$> filter (LT.isPrefixOf fpTxt) snapPaths


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
