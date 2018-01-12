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
import Data.Char
import Data.Maybe
import Text.HTML.Scalpel

xkcdHTML :: IO String
xkcdHTML = do
  itemM <- parse  
  case itemM of
    Just item -> 
      case (getItemTitle item, getItemSummary item) of
        (Just t, Just s) ->
          return $ "<div align=\"center\"><h2>" ++ t
          ++ "</h2></div>\n<div align=\"center\"><a href=\"https://xkcd.com\">" 
          ++ s ++ "</a><br>" ++ scrapeAlt s ++ "</div>\n<div align=\"center\">"
          ++ "<a href=\"https://explainxkcd.com\">confused?</a></div>"
        _ -> return ""
    Nothing -> return ""

-- TODO make this recover from errors better.
parse :: IO (Maybe Item)
parse = do
  xml <- xkcdXML
  let Just feed = parseFeedSource xml
  let Just update = getFeedDate feed
  currentTime <- zonedTimeToUTC <$> getZonedTime
  let time = mkNumber . toFeedDateStringUTC AtomKind $ currentTime
  let pubTime = mkNumber update
  return $ if time - 240000 < pubTime then Just (head . feedItems $ feed) else Nothing

xkcdXML :: IO B.ByteString
xkcdXML = simpleHttp "http://xkcd.com/atom.xml"

mkNumber :: String -> Int
mkNumber = read . foldl (\acc a -> if isNumber a then a:acc else acc) "" . reverse

scrapeAlt :: String -> String
scrapeAlt s = fromMaybe "" (scrapeStringLike s . attr "alt" $ "img")
