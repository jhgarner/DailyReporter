module Lib
    ( someFunc
    ) where

import Weather
import Xkcd
import Apod
import Text.Printf

someFunc :: IO ()
someFunc = do
  weather <- weatherHTML
  xkcd <- xkcdHTML
  apod <- apodHTML
  putStrLn weather
  putStrLn xkcd
  putStrLn apod
