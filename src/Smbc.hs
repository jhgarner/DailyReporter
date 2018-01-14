{-# LANGUAGE OverloadedStrings #-}

module Smbc
    ( smbcHTML
    ) where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Data.Maybe

smbcHTML :: IO String
smbcHTML = do
  xml <- smbcXML
  return . fromMaybe "" $ do
    item <- parse xml
    title <- getItemTitle item
    summary <- getItemSummary item
    Just $ "<center><h2>" ++ title ++ "</h2></center>\n<center><a href=\"https://smbc-comics.com\">" 
      ++ summary ++ "</a><br></center>"

parse :: B.ByteString -> Maybe Item
parse xml = parseFeedSource xml >>= (Just . head . feedItems)

smbcXML :: IO B.ByteString
smbcXML = simpleHttp "https://www.smbc-comics.com/rss.php"
