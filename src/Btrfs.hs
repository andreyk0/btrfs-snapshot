{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Btrfs
  ( findBtrfsSubvol
  ) where

import           Import
import           RIO.FilePath
import           RIO.Process
import           System.Directory
import           System.Process.Typed (nullStream)
import           Types


{- | Walk the path up until we hit subvolume boundary.
     Returns canonical absolute subvolume path.
-}
findBtrfsSubvol
  :: HasCLI env
  => FilePath
  -> RIO env (Either Text FilePath)
findBtrfsSubvol fPath' = do
  fPath <- liftIO $ canonicalizePath fPath'

  e <- proc "btrfs" ["subvol", "show", fPath]
       ( runProcess
       . setStdout nullStream
       . setStderr nullStream
       )
  case e
    of ExitSuccess   -> pure $ Right fPath
       ExitFailure _ ->
         case (fPath, takeDirectory fPath)
           of ("/", _)     -> pure $ Left "Failed to find btrfs subvol"
              (".", _)     -> pure $ Left "Failed to get canonical path"
              (_, fParent) -> findBtrfsSubvol fParent
