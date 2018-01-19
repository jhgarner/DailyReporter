{-# LANGUAGE OverloadedStrings #-}

module Buttersafe
    ( butterHtml
    ) where

import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.Scalpel
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Format
import System.Locale.Read

data Butter = Butter 
    { img :: String, title :: String, quote :: String }

butterHtml :: IO String
butterHtml = do 
  time <- zonedTimeToUTC <$> getZonedTime
  Just comic <- butterComicScraper
  locale <- getCurrentLocale
  let (Butter img title quote) = if isRightDay (formatTime locale "%A" time) then comic else Butter "" "" ""
  return $ "<center><h2>" ++ title ++ "</h2><br><img src=\"" 
    ++ img ++ "\"><br><p>" ++ quote ++ "</p></center>"

butterResponse :: IO B.ByteString
butterResponse = simpleHttp "http://buttersafe.com" 

butterComicScraper :: IO (Maybe Butter)
butterComicScraper = scrapeURL "http://buttersafe.com" scraper

scraper :: Scraper String Butter
scraper = do 
  title <- chroot ("h2" @: [hasClass "index-title"]) $ text "a"
  img <- chroot ("div" @: ["id" @= "comic"]) $ attr "src" "img"
  quote <- chroot ("div" @: [hasClass "entry"]) $ innerHTML anySelector
  return $ Butter img title quote

isRightDay :: String -> Bool
isRightDay "Tuesday" = True
isRightDay "Thursday" = True
isRightDay _ = False
