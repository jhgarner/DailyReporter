{-# LANGUAGE OverloadedStrings #-}
module Apod
  ( apodHTML
  ) where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BB
import Text.HTML.Scalpel
import Data.ByteString.Search
import Data.ByteString.Char8
import Data.Maybe

data Apod = Apod
  { img :: String
  , title :: String
  , explanation :: String
  }

apodHTML :: IO String
apodHTML = do 
  scrapedM <- scrapeSite
  return . fromMaybe "" 
    $ (\(Apod img title exp) -> "<center><a href=\"https://apod.nasa.gov/apod/astropix.html\">"
      ++"<h2>Astronomy Picture of the Day</h2></a>" ++ "<br>" ++ title 
      ++ "<br><br><a href=\"https://apod.nasa.gov/apod/" ++ img
      ++ "\"><img width=800 src=https://apod.nasa.gov/apod/" ++ img ++ "></a><br><br>"
      ++ exp ++ "</center>") <$> scrapedM

scrapeSite :: IO (Maybe Apod)
scrapeSite = do
  response <- apodResponse
  let fixed = replace ("<p>" :: BB.ByteString)  ("</p><p>" :: BB.ByteString) . B.toStrict $ response
  return $ scrapeStringLike ((unpack . B.toStrict $ fixed) :: String) scraper

apodResponse :: IO B.ByteString
apodResponse = simpleHttp "https://apod.nasa.gov/apod/astropix.html" 

scraper :: Scraper String Apod
scraper = do
  img <- attrs "href" "a"
  title <- html "b"
  explanation <- texts "p"
  return $ Apod (img !! 1) title (explanation !! 2)
