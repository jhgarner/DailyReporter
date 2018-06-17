{-# LANGUAGE OverloadedStrings #-}

module Qwantz
  ( qwantz
  ) where

import Data.Maybe
import Data.Text
import Text.HTML.Scalpel
import Utils

qwantz :: IO Text
qwantz = do
  (title, summary) <- getTitleAndSummary "http://www.qwantz.com/rssfeed.php"
  return . template htmlS $
      [("*title", title), ("*summary", scrapeImg summary), ("*alt", scrapeAlt summary)]

htmlS :: Text
htmlS =
  pack $
  "<div align=\"center\"><h2>*title</h2></div>\n<div align=\"center\">" ++
  "<a href=\"https://xkcd.com\">*summary</a> <p>*alt</p></div><br>"

scrapeAlt :: Text -> Text
scrapeAlt s = fromMaybe "" . scrapeStringLike s . attr "title" $ "img"

scrapeImg :: Text -> Text
scrapeImg s = fromMaybe "" . scrapeStringLike s . html $ "img" @: [hasClass "comic"]
