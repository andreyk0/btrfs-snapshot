{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Types

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
