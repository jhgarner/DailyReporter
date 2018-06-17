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

import Data.Text (Text)
import Data.Text.IO as T
import Control.Monad

someFunc :: IO ()
someFunc = do
  mapM_ (>>= T.putStrLn) sources

sources :: [IO Text]
sources = [weather, ec, apod, smbc, butter, pdl, qwantz, word, xkcd]
