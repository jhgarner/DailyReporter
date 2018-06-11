{-# LANGUAGE OverloadedStrings #-}

module Ec
  ( ecHTML
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

ecHTML :: IO Text
ecHTML = do
  (title, summary) <- getTitleAndSummary "https://existentialcomics.com/rss.xml"
  let fixedSummary = replace "//" "http://" summary
  return . template htmlS $
    fromList
      [("*title", title), ("*summary", fixedSummary)]

htmlS :: Text
htmlS =
  pack $
  "<div align=\"center\"><h2>*title</h2></div>\n<div align=\"center\">" ++
  "<a href=\"https://xkcd.com\">*summary</a> <p>*alt</p></div><br>"
