{-# LANGUAGE OverloadedStrings #-}

module Smbc
  ( smbc
  ) where

import Data.Text
import Utils

smbc :: IO Text
smbc = do
  (title, summary) <- getTitleAndSummary "https://www.smbc-comics.com/rss.php"
  return . template html $ [("*title", title), ("*summary", summary)]

html = "<center><h2>*title</h2></center><br><center>*summary<br></center>"
