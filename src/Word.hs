{-# LANGUAGE OverloadedStrings #-}

module Word
  ( wordHTML
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Map
import Data.Maybe
import Data.Text
import Data.Time.Clock
import Data.Time.LocalTime
import Network.HTTP.Conduit (simpleHttp)
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.Feed.Util
import Text.HTML.Scalpel
import Utils

wordHTML :: IO Text
wordHTML = do
  (title, summary) <- getTitleAndSummary "https://www.merriam-webster.com/wotd/feed/rss2"
  return . template htmlS $
    fromList
      [("*title", title), ("*summary", summary)]

htmlS :: Text
htmlS =
  pack $
  "<div align=\"center\"><h2>*title</h2></div>\n<div align=\"center\">*summary"
