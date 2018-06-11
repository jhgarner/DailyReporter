{-# LANGUAGE OverloadedStrings #-}

module Smbc
  ( smbcHTML
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Map
import Data.Maybe
import Data.Text
import Data.Time.Clock
import Network.HTTP.Conduit (simpleHttp)
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Utils

smbcHTML :: IO Text
smbcHTML = do
  (title, summary) <- getTitleAndSummary "https://www.smbc-comics.com/rss.php"
  return . template html $ fromList [("*title", title), ("*summary", summary)]

html = "<center><h2>*title</h2></center><br><center>*summary<br></center>"
