{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Btrfs
  ( findBtrfsSubvol
  ) where

import           Import
import           RIO.Process
import           Types


-- | Walk the path up until we hit subvolume boundary
findBtrfsSubvol
  :: HasCLI env
  => FilePath
  -> RIO env FilePath
findBtrfsSubvol fPath = do
  e <-  proc "btrfs" ["subvol", "show", fPath] runProcess
  error $ show e
