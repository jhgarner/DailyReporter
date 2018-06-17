{-# LANGUAGE OverloadedStrings #-}

module Word
  ( word
  ) where

import Data.Text
import Utils

word :: IO Text
word = do
  (title, summary) <- getTitleAndSummary "https://www.merriam-webster.com/wotd/feed/rss2"
  return . template htmlS $ [("*title", title), ("*summary", summary)]

htmlS :: Text
htmlS =
  pack $
  "<div align=\"center\"><h2>*title</h2></div>\n<div align=\"center\">*summary"
