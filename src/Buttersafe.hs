{-# LANGUAGE OverloadedStrings #-}

module Buttersafe
  ( butterHtml
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Map
import Data.Text
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Network.HTTP.Conduit (simpleHttp)
import System.Locale.Read
import Text.HTML.Scalpel
import Utils

data Butter = Butter
  { img :: Text
  , title :: Text
  , quote :: Text
  }

butterHtml :: IO Text
butterHtml = do
  (title, _) <-
    getTitleAndSummary "http://feeds.feedburner.com/Buttersafe?format=xml"
  Just comic <- butterComicScraper
  return . template htmlS $
    fromList [("*title", title), ("*img", img comic), ("*quote", quote comic)]

htmlS = "<center><h2>*title</h2<br><img src=\"*img\"><br><p>*quote</p></center>"

butterComicScraper :: IO (Maybe Butter)
butterComicScraper = scrapeURL "http://buttersafe.com" scraper

scraper :: Scraper String Butter
scraper = do
  title <- chroot ("h2" @: [hasClass "index-title"]) $ text "a"
  img <- chroot ("div" @: ["id" @= "comic"]) $ attr "src" "img"
  quote <- chroot ("div" @: [hasClass "entry"]) $ innerHTML anySelector
  return $ Butter (pack img) (pack title) (pack quote)
