module Lib
    ( someFunc
    ) where

import Weather
import Text.Printf

someFunc :: IO ()
someFunc = do
  weather <- weatherHTML
  putStrLn weather


