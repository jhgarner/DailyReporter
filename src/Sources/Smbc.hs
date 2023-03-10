{-# LANGUAGE OverloadedStrings #-}

module Sources.Smbc (smbc) where

import Data.ByteString (ByteString, readFile)
import Data.Map (Map, insert)
import Data.Text (Text)
import Parser.HtmlParser (extractHtml)
import Utils (getTitleAndSummary)

smbc :: IO (Map Text Text)
smbc = do
  guide <- Data.ByteString.readFile "parsers/smbc.json"
  (title, summary) <- getTitleAndSummary "https://www.smbc-comics.com/rss.php"
  pure $ insert "*title" title $ extractHtml guide summary