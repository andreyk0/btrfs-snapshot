{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}


module Btrfs.Types where


import           Import


newtype BtrfsException = BtrfsException Text deriving (Eq,Show,Generic)
instance Exception BtrfsException


data BtrfsFilePath = BtrfsFilePath
  { bfpFilePath          :: FilePath -- ^ original file path
  , bfpCanonicalFilePath :: FilePath -- ^ canonical file path
  , bfpSubvol            :: FilePath -- ^ nearest parent btrfs subvolume
  , bfpMountPoint        :: FilePath -- ^ mount point of the btrfs filesysem containing this path
  } deriving (Eq, Ord, Show)
