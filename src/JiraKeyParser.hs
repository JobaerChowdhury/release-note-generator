{-# LANGUAGE OverloadedStrings #-}

module JiraKeyParser (extractKey) where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8
import Data.Word

data JK = JK String String deriving (Show)

parseJK :: Parser JK
parseJK = do
  p1 <- many' ksep
  _ <- many' space
  _ <- many' $ char '-'
  _ <- many' space
  k1 <- many1 digit
  return $ JK p1 k1

ksep :: Parser Char
ksep = satisfy isSep
  where isSep c = c /= ' ' && c /= '-' && c /= ':'

extractKey :: String -> String
extractKey s = case parseOnly parseJK (pack s) of
                    Left _ -> "Nothing"
                    Right (JK a b) -> a ++ "-" ++ b

main :: IO ()
main = print $ parseOnly parseJK "MM-123"

