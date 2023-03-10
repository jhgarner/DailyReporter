{-# LANGUAGE OverloadedStrings #-}

module Sources.Word (word) where

import Data.Map (Map, fromList)
import Data.Text (Text)
import Utils (getTitleAndSummary)
import Data.Text.Encoding (decodeUtf8)

word :: IO (Map Text Text)
word = do
  (title, summary) <- getTitleAndSummary "https://www.merriam-webster.com/wotd/feed/rss2"
  pure $ fromList [("*title", title), ("*summary", decodeUtf8 summary)]