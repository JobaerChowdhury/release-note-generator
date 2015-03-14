{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Main where 

import JiraKeyParser

import Control.Lens ((&), (^.), (^?), (.~), (^?!), to, (^..))
import Control.Lens.Setter ((?~))

import Data.Aeson.Lens (_Array, key, _String)
import Data.Traversable (traverse)
import Data.Text (Text, unpack)
import Data.List (nub, isInfixOf)
import qualified Data.Configurator as C
import Data.Configurator.Types
import qualified Data.ByteString as B

import Network.Wreq
import System.Environment

-- todo Move all url, user/pass in a configuration file. And read from there
-- todo error handling

data AppConfig = AppConfig {
  ghUser :: B.ByteString,
  ghPass :: B.ByteString,
  ghUrl :: String,
  jiraUser :: B.ByteString,
  jiraPass :: B.ByteString,
  jiraBrowse :: String,
  jiraUrl :: String
} deriving (Show)

readConfig :: IO AppConfig
readConfig = do
  putStrLn "Reading configuration file config.cfg ..."
  let fp = C.Required "config.cfg" :: C.Worth FilePath
  config <- C.load [fp]
  gu <- C.require config "gh_user" :: IO B.ByteString
  gp <- C.require config "gh_pass" :: IO B.ByteString
  gurl <- C.require config "gh_url" :: IO String
  ju <- C.require config "jira_user" :: IO B.ByteString
  jp <- C.require config "jira_pass" :: IO B.ByteString
  jb <- C.require config "jira_browse_url" :: IO String
  jurl <- C.require config "jira_url" :: IO String
  return $ AppConfig gu gp gurl ju jp jb jurl

showHint :: IO ()
showHint = do 
  putStrLn "Please invoke the program like this"
  putStrLn "run <previous-tag> <current-tag>"

-- we are certain that the list contains more than 2 element
processRequest :: [String] -> IO ()
processRequest args = do
  let tag1 = head args
  let tag2 = head $ tail args
  cfg <- readConfig
  result <- fetchNotes cfg tag1 tag2
  mapM_ putStrLn result

-- takes two tag names and prepare the release notes
fetchNotes :: AppConfig -> String -> String -> IO [String]
fetchNotes cfg tagFrom tagTo = do
  putStrLn "Fetching the notes"
  comments <- request_github cfg tagFrom tagTo
  summaries <- process_comments cfg comments
  return summaries

-- get a list of comments, find their keys, return the jira summary
-- if key not found then return as it is
process_comments :: AppConfig -> [String] -> IO [String]
process_comments cfg cs = do
  let paired = pairWithKey cs
  let keys = nub $ map snd $ filter (\(_, s) -> s /= "Nothing") paired
  summaries <- mapM (jira_summary_with_url cfg) keys
  let verbatim = filter (\s -> not (isInfixOf "Merge" s)) $ map fst $ filter (\(_, s) -> s == "Nothing") paired
  return $ summaries ++ verbatim

request_github :: AppConfig -> String -> String -> IO [String]
request_github cfg fromTag toTag = do
  let gu = ghUser cfg
  let gp = ghPass cfg
  let gurl = ghUrl cfg
  let opts = defaults & auth ?~ basicAuth gu gp
  let github_url = gurl ++ fromTag ++ "..." ++ toTag
  v <- asValue =<< getWith opts github_url
  let msgs = v ^.. responseBody . key "commits" . _Array . traverse . to (\o ->  commitMsg o)  
  return $ map unpack $ msgs
  where commitMsg o = o ^?! key "commit" . key "message" . _String

jira_summary_with_url :: AppConfig -> String -> IO String
jira_summary_with_url cfg jk = do
  summary <- get_jira_summary cfg jk
  let jbu = jiraBrowse cfg
  return $ jbu ++ jk ++ " : " ++ summary

-- get jira summary for a specific key
get_jira_summary :: AppConfig -> String -> IO String
get_jira_summary cfg jirakey = do
  let ju = jiraUser cfg
  let jp = jiraPass cfg
  let jurl = jiraUrl cfg
  let opts = defaults & auth ?~ basicAuth ju jp
  let jiraurl = jurl ++ jirakey ++ "?fields=key,summary"
  v <- asValue =<< getWith opts jiraurl
  let msg = v ^?! responseBody . key "fields" . key "summary" . _String
  return $ unpack msg

pairWithKey :: [String] -> [(String, String)]
pairWithKey = map (\s -> (s, (extractKey s)))

main :: IO ()
main = do
  args <- getArgs
  if ((length args) < 2) then showHint else processRequest args
	