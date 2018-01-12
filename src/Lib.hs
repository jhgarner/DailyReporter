module Lib
    ( someFunc
    ) where

import Weather
import Xkcd
import Text.Printf

someFunc :: IO ()
someFunc = do
  weather <- weatherHTML
  xkcd <- xkcdHTML
  putStrLn weather
  putStrLn xkcd
