{-# LANGUAGE OverloadedStrings #-}

module Xkcd
  ( xkcd
  ) where

import Data.Maybe
import Data.Text
import Data.Text.IO
import Text.HTML.Scalpel
import Utils

-- |The final html text generated by scraping xkcd RSS feed.
xkcd :: IO Text
xkcd = do
  h <- htmlS
  (title, summary) <- getTitleAndSummary "https://xkcd.com/atom.xml"
  return (template
          [("*title", title)
         , ("*summary", summary)
         , ("*alt", scrapeAlt summary)] h)

-- |Templating for xkcd section. (templates/Xkcd.html)
htmlS :: IO Text
htmlS = Data.Text.IO.readFile "templates/Xkcd.html"

-- |Scrape image from xkcd rss feed.
scrapeAlt :: Text -> Text
scrapeAlt s =
    fromMaybe "" .
    scrapeStringLike s .
    attr "alt" $ "img"
