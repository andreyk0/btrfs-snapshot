{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Btrfs.Types where


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
