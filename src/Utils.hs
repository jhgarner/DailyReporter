{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( template,
    getHttp,
    getLatestItem,
    getTitleAndSummary,
    getTitle,
    readConfig,
  )
where

import Control.Applicative
import qualified Data.ByteString as B
import Data.Map as Map
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Encoding.Error as E
import qualified Data.Text.Lazy as L
import Data.Time
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types

-- | Template function (USED EVERWHERE) for replacing keys in template.
--  TODO: Replace with something better than * (mustache templating)
template :: [(Text, Text)] -> Text -> Text
template k t = (Map.foldlWithKey' replaceOrEmpty t . fromList) k

-- | Safe replace on text.
replaceOrEmpty :: Text -> Text -> Text -> Text
replaceOrEmpty acc "" _ = acc
replaceOrEmpty acc k v = replace k v acc

-- | Way to go straight from String URL to text response.
getHttp :: String -> IO Text
getHttp url = do
  initReq <- parseRequest url
  let req = setRequestResponseTimeout responseTimeoutNone initReq
  decodeUtf8With E.lenientDecode . getResponseBody <$> httpBS req

-- | Returns latest item from RSS feed, but only if it's within the last 24
--  hours.
getLatestItem :: Text -> Item
getLatestItem xml =
  Prelude.head $ feedItems $ fromJust $ parseFeedString (unpack xml) -- make it a string. Parse it.

-- | Gets the title and summary from XML given. Common in RSS feeds.
getTitleAndSummary :: String -> IO (Text, Text)
getTitleAndSummary name = do
  item <- getLatestItem <$> getHttp name
  return $ fromMaybe mempty $ liftA2 (,) (getItemTitle item) (getItemSummary item)

getTitle :: String -> IO Text
getTitle name = do
  item <- getLatestItem <$> getHttp name
  return $ fromMaybe mempty $ getItemTitle item

-- | Automatic parser for getting config from file into mail config.
readConfig :: (Read a) => String -> IO a
readConfig = fmap read . readFile . ("configs/" ++)
