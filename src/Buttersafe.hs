{-# LANGUAGE OverloadedStrings #-}

module Buttersafe
  ( butter
  ) where

import Data.Map
import Data.Text
import Text.HTML.Scalpel
import Utils

data Butter = Butter
  { img :: Text
  , quote :: Text
  }

butter :: IO Text
butter = do
  (title, _) <-
    getTitleAndSummary "http://feeds.feedburner.com/Buttersafe?format=xml"
  Just comic <- butterComicScraper
  return . template htmlS $
    [("*title", title), ("*img", img comic), ("*quote", quote comic)]

htmlS = "<center><h2>*title</h2<br><img src=\"*img\"><br><p>*quote</p></center>"

butterComicScraper :: IO (Maybe Butter)
butterComicScraper = scrapeURL "http://buttersafe.com" scraper

scraper :: Scraper String Butter
scraper = do
  img <- chroot ("div" @: ["id" @= "comic"]) $ attr "src" "img"
  quote <- chroot ("div" @: [hasClass "entry"]) $ innerHTML anySelector
  return $ Butter (pack img) (pack quote)
