{-# LANGUAGE OverloadedStrings #-}

module Xkcd
  ( xkcdHTML
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

xkcdHTML :: IO Text
xkcdHTML = do
  (title, summary) <- getTitleAndSummary "https://xkcd.com/atom.xml"
  return . template htmlS $
    fromList
      [("*title", title), ("*summary", summary), ("*alt", scrapeAlt summary)]

htmlS :: Text
htmlS =
  pack $
  "<div align=\"center\"><h2>*title</h2></div>\n<div align=\"center\">" ++
  "<a href=\"https://xkcd.com\">*summary</a><br>*alt</div><br>" ++
  "<div align=\"center\"><a href=\"https://explainxkcd.com\">confused?</a></div>"

scrapeAlt :: Text -> Text
scrapeAlt s = fromMaybe "" (scrapeStringLike s . attr "alt" $ "img")
