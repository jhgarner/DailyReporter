module Lib
  ( someFunc
  ) where

import Apod
import Buttersafe
import Smbc
import Weather
import Xkcd
import Ec
import PDL
import Qwantz
import Word

import Data.Text

-- import PDL
someFunc :: IO ()
someFunc = do
  weather <- weatherHTML
  ec <- ecHTML
  apod <- apodHTML
  smbc <- smbcHTML
  butter <- butterHtml
  pdl <- pdlHTML
  qwantz <- qwantzHTML
  word <- wordHTML
  xkcd <- xkcdHTML
  putStrLn $ unpack weather
  putStrLn apod
  putStrLn $ unpack word
  putStrLn $ unpack xkcd
  putStrLn $ unpack smbc
  putStrLn $ unpack ec
  putStrLn $ unpack butter
  putStrLn $ unpack pdl
  putStrLn $ unpack qwantz
