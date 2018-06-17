{-# LANGUAGE OverloadedStrings #-}

module Ec
  ( ec
  ) where

import Data.Text
import Utils

ec :: IO Text
ec = do
  (title, summary) <- getTitleAndSummary "https://existentialcomics.com/rss.xml"
  let fixedSummary = replace "//" "http://" summary
  return . template htmlS $ [("*title", title), ("*summary", fixedSummary)]

htmlS :: Text
htmlS =
  pack $
  "<div align=\"center\"><h2>*title</h2></div>\n<div align=\"center\">" ++
  "<a href=\"https://xkcd.com\">*summary</a> <p>*alt</p></div><br>"
