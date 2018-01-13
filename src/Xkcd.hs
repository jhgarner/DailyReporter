{-# LANGUAGE OverloadedStrings #-}

module Xkcd
    ( xkcdHTML
    ) where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Util
import Text.Feed.Types
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Char
import Data.Maybe
import Text.HTML.Scalpel

xkcdHTML :: IO String
xkcdHTML = do
  xml <- xkcdXML
  currentTime <- zonedTimeToUTC <$> getZonedTime
  return . fromMaybe "" $ do
    item <- parse xml currentTime
    title <- getItemTitle item
    summary <- getItemSummary item
    Just $ "<div align=\"center\"><h2>" ++ title
      ++ "</h2></div>\n<div align=\"center\"><a href=\"https://xkcd.com\">" 
      ++ summary ++ "</a><br>" ++ scrapeAlt summary ++ "</div>\n<div align=\"center\">"
      ++ "<a href=\"https://explainxkcd.com\">confused?</a></div>"

parse :: B.ByteString -> UTCTime -> Maybe Item
parse xml currentTime = do
  feed <- parseFeedSource xml
  update <- getFeedDate feed
  let time = mkNumber . toFeedDateStringUTC AtomKind $ currentTime
  let pubTime = mkNumber update
  if time - 240000 < pubTime then Just (head . feedItems $ feed) else Nothing

xkcdXML :: IO B.ByteString
xkcdXML = simpleHttp "http://xkcd.com/atom.xml"

mkNumber :: String -> Int
mkNumber = read . foldl (\acc a -> if isNumber a then a:acc else acc) "" . reverse

scrapeAlt :: String -> String
scrapeAlt s = fromMaybe "" (scrapeStringLike s . attr "alt" $ "img")
