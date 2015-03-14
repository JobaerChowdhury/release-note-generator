{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module ConfiguratorTest where

import Control.Exception
import qualified Data.Configurator as C
import Data.Configurator.Types

main :: IO ()
main = do
  let fp = C.Required "config.cfg" :: C.Worth FilePath
  config <- C.load [fp]
  jira_url <- C.require config "jira_url" :: IO String
  putStrLn jira_url
  putStrLn "Hello Configurator..."