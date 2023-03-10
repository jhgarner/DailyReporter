{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Control.Applicative (Applicative (liftA2))
import Data.Foldable.WithIndex (ifoldl', FoldableWithIndex (ifoldr'))
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, replace, unpack)
import Data.Text.Encoding (decodeUtf8With, decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding.Error as E
import Network.HTTP.Conduit (parseRequest, responseTimeoutMicro)
import Network.HTTP.Simple
  ( getResponseBody,
    httpBS,
    parseRequest,
    setRequestResponseTimeout,
  )
import Text.Feed.Import (parseFeedString)
import Text.Feed.Query (feedItems, getItemSummary, getItemTitle)
import Text.Feed.Types (Item)
import Data.ByteString (ByteString)

-- | Template function for replacing keys in template.
--  TODO: Replace with something better than * (mustache templating)
template :: Text -> Map Text Text -> Text
template = ifoldr' replace

-- | Way to go straight from String URL to text response.
getHttp :: String -> IO ByteString
getHttp url = do
  initReq <- parseRequest url
  let req = setRequestResponseTimeout (responseTimeoutMicro $ floor 3e7) initReq
  getResponseBody <$> httpBS req

-- | Returns latest item from RSS feed, but only if it's within the last 24
--  hours.
getLatestItem :: Text -> Item
getLatestItem xml =
  Prelude.head $ feedItems $ fromJust $ parseFeedString (unpack xml) -- make it a string. Parse it.

-- | Gets the title and summary from XML given. Common in RSS feeds.
getTitleAndSummary :: String -> IO (Text, ByteString)
getTitleAndSummary name = do
  item <- getLatestItem . decodeUtf8 <$> getHttp name
  return $ fromMaybe mempty $ liftA2 (,) (getItemTitle item) (encodeUtf8 <$> getItemSummary item)

getTitle :: String -> IO Text
getTitle name = do
  item <- getLatestItem . decodeUtf8 <$> getHttp name
  return $ fromMaybe mempty $ getItemTitle item

-- | Automatic parser for getting config from file into mail config.
readConfig :: (Read a) => String -> IO a
readConfig = fmap read . readFile . ("configs/" ++)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mBool mTrue mFalse = mBool >>= \condition -> if condition then mTrue else mFalse