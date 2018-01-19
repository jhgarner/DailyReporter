module Lib
    ( someFunc
    ) where

import Weather
import Xkcd
import Apod
import Smbc
import Buttersafe

someFunc :: IO ()
someFunc = do
  -- weather <- weatherHTML
  -- xkcd <- xkcdHTML
  -- apod <- apodHTML
  -- smbc <- smbcHTML
  butter <- butterHtml
  -- putStrLn weather
  -- putStrLn xkcd
  -- putStrLn apod
  -- putStrLn smbc
  putStrLn butter
