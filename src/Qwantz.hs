{-# LANGUAGE OverloadedStrings #-}

module Qwantz
  ( qwantzHTML
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

qwantzHTML :: IO Text
qwantzHTML = do
  (title, summary) <- getTitleAndSummary "http://www.qwantz.com/rssfeed.php"
  return . template htmlS $
    fromList
      [("*title", title), ("*summary", scrapeImg summary), ("*alt", scrapeAlt summary)]

htmlS :: Text
htmlS =
  pack $
  "<div align=\"center\"><h2>*title</h2></div>\n<div align=\"center\">" ++
  "<a href=\"https://xkcd.com\">*summary</a> <p>*alt</p></div><br>"

scrapeAlt :: Text -> Text
scrapeAlt s = fromMaybe "" (scrapeStringLike s . attr "title" $ "img")

scrapeImg :: Text -> Text
scrapeImg s = fromMaybe "" (scrapeStringLike s . html $ "img" @: [hasClass "comic"])
