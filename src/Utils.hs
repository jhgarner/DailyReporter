{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( template
  , getHttp
  , getLatestItem
  , getTitleAndSummary
  --, getConfig
  , readConfig
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Map as Map

import Data.Maybe
import Data.Text
import qualified Data.Text.Lazy as L
import Data.Text.Encoding
import qualified Data.Text.Encoding.Error as E

import Control.Applicative

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
getLatestItem :: Text -> Item
getLatestItem xml =
  Prelude.head $ feedItems $ fromJust $ parseFeedString (unpack xml) -- make it a string. Parse it.

-- |Gets the title and summary from XML given. Common in RSS feeds.
getTitleAndSummary :: String -> IO (Text, Text)
getTitleAndSummary name = do
  item <- getLatestItem <$> getHttp name
  return $ fromMaybe mempty $ liftA2 (,) (getItemTitle item) (getItemSummary item)

-- |Automatic parser for getting config from file into mail config.
readConfig :: (Read a) => String -> IO a
readConfig = fmap read . readFile . ("configs/" ++)
