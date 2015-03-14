-- Examples of handling for JSON responses
--
-- This library provides several ways to handle JSON responses

{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Control.Lens ((&), (^.), (^?), (.~), (^?!), to, (^..))
import Control.Lens.Setter ((?~))
import Data.Aeson (FromJSON)
import Data.Aeson.Lens (_Array, key, _String)
import Data.Map (Map)
import Data.Traversable (traverse)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Control.Exception as E

import Network.Wreq


-- This Haskell type corresponds to the structure of a response body
-- from httpbin.org.

data GetBody = GetBody {
    headers :: Map Text Text
  , args :: Map Text Text
  , origin :: Text
  , url :: Text
  } deriving (Show, Generic)

-- Get GHC to derive a FromJSON instance for us.

instance FromJSON GetBody



-- We expect this to succeed.

basic_asJSON :: IO ()
basic_asJSON = do
  let opts = defaults & param "foo" .~ ["bar"]
  r <- asJSON =<< getWith opts "http://httpbin.org/get"

  -- The fact that we want a GetBody here will be inferred by our use
  -- of the "headers" accessor function.

  putStrLn $ "args: " ++ show (args (r ^. responseBody))



-- The response we expect here is valid JSON, but cannot be converted
-- to an [Int], so this will throw a JSONError.

failing_asJSON :: IO ()
failing_asJSON = do
  (r :: Response [Int]) <- asJSON =<< get "http://httpbin.org/get"
  putStrLn $ "response: " ++ show (r ^. responseBody)



-- This demonstrates how to catch a JSONError.

failing_asJSON_catch :: IO ()
failing_asJSON_catch =
  failing_asJSON `E.catch` \(e :: JSONError) -> print e



-- Because asJSON is parameterized over MonadThrow, we can use it with
-- other instances.
--
-- Here, instead of throwing an exception in the IO monad, we instead
-- evaluate the result as an Either:
--
-- * if the conversion fails, the Left constructor will contain
--   whatever exception describes the error
--
-- * if the conversion succeeds, the Right constructor will contain
--   the converted response

either_asJSON :: IO ()
either_asJSON = do
  r <- get "http://httpbin.org/get"

  -- This first conversion attempt will fail, but because we're using
  -- Either, it will not throw an exception that kills execution.
  let failing = asJSON r :: Either E.SomeException (Response [Int])
  print failing

  -- Our second conversion attempt will succeed.
  let succeeding = asJSON r :: Either E.SomeException (Response GetBody)
  print succeeding



-- The lens package defines some handy combinators for use with the
-- aeson package, with which we can easily traverse parts of a JSON
-- response.

lens_aeson :: IO ()
lens_aeson = do
  r <- get "http://httpbin.org/get"
  print $ r ^? responseBody . key "headers" . key "User-Agent"

  -- If we maintain the ResponseBody as a ByteString, the lens
  -- combinators will have to convert the body to a Value every time
  -- we start a new traversal.

  -- When we need to poke at several parts of a response, it's more
  -- efficient to use asValue to perform the conversion to a Value
  -- once.

  let opts = defaults & param "baz" .~ ["quux"]
  v <- asValue =<< getWith opts "http://httpbin.org/get"
  print $ v ^? responseBody . key "args" . key "baz"

basic_auth :: IO ()
basic_auth = do 
  let opts = defaults & auth ?~ basicAuth "user" "pass"
  r <- getWith opts "https://httpbin.org/basic-auth/user/pass"
  print $ r ^. responseBody

github_test :: IO [Text]
github_test = do  
  let opts = defaults & auth ?~ basicAuth "user" "pass"
  v <- asValue =<< getWith opts "https://api.github.com/repos/myrepo/compare/TAG-3.10.1...TAG-3.10.3"
  let msgs = v ^.. responseBody . key "commits" . _Array . traverse . to (\o ->  commitMsg o)
  putStrLn (show (length msgs))
  return msgs
  where commitMsg o = o ^?! key "commit" . key "message" . _String

-- get jira summary for a specific key
get_jira_summary :: String -> IO Text
get_jira_summary jirakey = do  
  let opts = defaults & auth ?~ basicAuth "jirauser" "jirapass"
  let jiraurl = "https://company.atlassian.net/rest/api/latest/issue/" ++ jirakey ++ "?fields=key,summary"
  v <- asValue =<< getWith opts jiraurl
  let msg = v ^?! responseBody . key "fields" . key "summary" . _String
  return msg


main :: IO ()
main = do
  basic_asJSON
  failing_asJSON_catch
  either_asJSON
  lens_aeson