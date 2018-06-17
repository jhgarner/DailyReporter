{-# LANGUAGE OverloadedStrings #-}

module Apod
  ( apod
  ) where

import Data.Maybe
import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.Scalpel

import Data.Text
import Data.Text.Encoding

import Utils

data Apod = Apod
  { img :: String
  , title :: String
  , explanation :: String
  }

apod :: IO Text
apod = scrapeSite >>= return . template htmlS . fromMaybe []

htmlS :: Text
htmlS = "<center><a href=\"https://apod.nasa.gov/apod/astropix.html\"><h2>Astronomy Picture of the Day</h2></a><br>*title<br><br><a href=\"https://apod.nasa.gov/apod/*img\"><img width=800 src=https://apod.nasa.gov/apod/*img></a><br><br>*exp</center>"

scrapeSite :: IO (Maybe [(Text, Text)])
scrapeSite = do
  response <- replace "<p>" "</p><p>" <$> source
  return $ scrapeStringLike response scraper

source :: IO Text
source = getHttp "https://apod.nasa.gov/apod/astropix.html"

scraper :: Scraper Text [(Text, Text)]
scraper = do
  img <- (!! 1) <$> attrs "href" "a"
  title <- html "b"
  explanation <- (!! 2) <$> texts "p"
  return $ ("*img", img) : ("*title", title) : ("*exp", explanation) : []
