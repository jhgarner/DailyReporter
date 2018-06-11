{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( template
  , getFeed
  , getLatestItem
  , getTitleAndSummary
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Map as Map
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Data.Time
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Network.HTTP.Conduit (simpleHttp)
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.Feed.Util

template :: Text -> Map.Map Text Text -> Text
template t newStrings = Map.foldlWithKey' replaceOrEmpty t newStrings

replaceOrEmpty :: Text -> Text -> Text -> Text
replaceOrEmpty _ _ "" = ""
replaceOrEmpty acc k v = replace k v acc

getFeed :: String -> IO Text
getFeed = fmap (decodeUtf8 . B.toStrict) . simpleHttp

getLatestItem :: Text -> UTCTime -> Maybe Item
getLatestItem xml time = do
  feed <- parseFeedString $ unpack xml
  fstItem <- Just . Prelude.head $ feedItems feed
  update <- getItemPublishDate fstItem
  date <- update
  if addUTCTime (24 * 60 * 60) date >= time
    then Just fstItem
    else Nothing

getTitleAndSummary :: String -> IO (Text, Text)
getTitleAndSummary name = do
  xml <- getFeed name
  time <- getCurrentTime
  return . fromMaybe ("", "") $ do
    item <- getLatestItem xml time
    title <- getItemTitle item
    summary <- getItemSummary item
    return (title, summary)
