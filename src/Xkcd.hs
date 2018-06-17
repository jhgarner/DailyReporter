{-# LANGUAGE OverloadedStrings #-}

module Xkcd
  ( xkcd
  ) where

import Data.Maybe
import Data.Text
import Text.HTML.Scalpel
import Utils

xkcd :: IO Text
xkcd = do
  (title, summary) <- getTitleAndSummary "https://xkcd.com/atom.xml"
  return $ template htmlS
      [("*title", title), ("*summary", summary), ("*alt", scrapeAlt summary)]

htmlS :: Text
htmlS =
  pack $
  "<div align=\"center\"><h2>*title</h2></div>\n<div align=\"center\">" ++
  "<a href=\"https://xkcd.com\">*summary</a><br>*alt</div><br>" ++
  "<div align=\"center\"><a href=\"https://explainxkcd.com\">confused?</a></div>"

scrapeAlt :: Text -> Text
scrapeAlt s = fromMaybe "" . scrapeStringLike s . attr "alt" $ "img"
