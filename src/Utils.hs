{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( template
  , getHttp
  , getLatestItem
  , getTitleAndSummary
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Map as Map

import Data.Maybe
import Data.Text
import qualified Data.Text.Lazy as L
import Data.Text.Encoding
import qualified Data.Text.Encoding.Error as E

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types

import Data.Time

import Network.HTTP.Conduit (simpleHttp)

-- |Template function (USED EVERWHERE) for replacing keys in template.
-- TODO: Replace with something better than * (mustache templating)
template :: [(Text, Text)] -> Text -> Text
template k t = (Map.foldlWithKey' replaceOrEmpty t . fromList) k

-- |Safe replace on text.
replaceOrEmpty :: Text -> Text -> Text -> Text
replaceOrEmpty _ _ "" = ""
replaceOrEmpty acc k v = replace k v acc


-- |Way to go straight from String URL to text response.
getHttp :: String -> IO Text
getHttp = fmap (decodeUtf8With E.lenientDecode . B.toStrict) . simpleHttp


-- |Returns latest item from RSS feed, but only if it's within the last 24
-- hours.
getLatestItem :: Text -> UTCTime -> Maybe Item
getLatestItem xml time = do
  feed <- parseFeedSource $ L.fromStrict xml -- make it a string. Parse it.
  fstItem <- Just . Prelude.head $ feedItems feed -- Get first item.
  updateTime <- getItemPublishDate fstItem
  date <- updateTime
  -- Check if the time of RSS feed + 24 hours is less than now.
  if addUTCTime (24 * 60 * 60) date >= time
    then Just fstItem
    else Nothing

-- |Gets the title and summary from XML given. Common in RSS feeds.
getTitleAndSummary :: String -> IO (Text, Text)
getTitleAndSummary name = do
  xml <- getHttp name
  time <- getCurrentTime
  return . fromMaybe ("", "") $ do
    item <- getLatestItem xml time
    title <- getItemTitle item
    summary <- getItemSummary item
    return (title, summary)
