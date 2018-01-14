module Lib
    ( someFunc
    ) where

import Weather
import Xkcd
import Apod
import Smbc

someFunc :: IO ()
someFunc = do
  weather <- weatherHTML
  xkcd <- xkcdHTML
  apod <- apodHTML
  smbc <- smbcHTML
  putStrLn weather
  putStrLn xkcd
  putStrLn apod
  putStrLn smbc
