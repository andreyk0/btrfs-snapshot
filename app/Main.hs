{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import           Import
import           Options.Applicative.Simple
import qualified Paths_btrfs_snapshot
import           RIO.Process
import           Run
import           Types

main :: IO ()
main = do
  (opts, cmd) <- simpleOptions
    $(simpleVersion Paths_btrfs_snapshot.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    ( do addCommand "create" "Create a new snapshot for a given path"
           CLISnapshotCreate
           (SnapshotCreate <$> some (argument str (metavar "PATHs...")))

         addCommand "transfer" "Transfer snapshotted subvolumes to another location"
           CLISnapshotTransfer
           (SnapshotTransfer
              <$> strOption ( long "from"
                            <> short 'f'
                            <> metavar "FROM_PATH"
                            <> help "Transfer snapshots from FROM_PATH" )

              <*> strOption ( long "to"
                            <> short 't'
                            <> metavar "TO_PATH"
                            <> help "Transfer snapshots to TO_PATH" )
           )
    )

  lo <- logOptionsHandle stderr (opts ^. optionsVerbose)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { _appLogFunc = lf
          , _appProcessContext = pc
          , _appOptions = opts
          , _appCommand = cmd
          }
     in runRIO app run
