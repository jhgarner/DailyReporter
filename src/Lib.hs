{-# LANGUAGE TypeApplications #-}

module Lib
  ( mailReport,
    printEmailHtml
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
import Email

import Data.Hashable
import Control.Exception
import Control.Monad

import Data.Text (Text, pack, intercalate)
-- import Data.Text as T
import qualified Data.Text.IO as I

-- data SourcesConfig = Config {
--     sources :: [IO Text]
-- } deriving (Read)

-- |Prints email html compiled from `sources`
-- Html items are simply appended without additional formatting.
-- TODO: Allow user config for sources.
-- TODO: Allow easier creation for new sources of standard RSS type.
printEmailHtml :: IO ()
printEmailHtml = do
  s <- sequence sources
  filtered <- removeOld s
  addNew s
  mapM_ I.putStrLn filtered

-- |Takes sources and compiles it into a single html. Then it emails it!
mailReport :: IO ()
mailReport = do
  s <- sequence sources
  filtered <- removeOld s
  addNew s
  mail $ intercalate (pack "\n") filtered

-- |List of sources.
sources :: [IO Text]
sources = [weather, ec, apod, smbc, butter, pdl, qwantz, word, xkcd]

removeOld :: [Text] -> IO [Text]
removeOld texts = handle @IOException (\_ -> return texts) $ do
  cache <- read @[Int] <$> readFile "cache.txt"
  when (length cache /= length texts) $ fail "Bad cache"
  return $ fmap snd $ filter (\(h, t) -> h /= hash t) $ zip cache texts

addNew :: [Text] -> IO ()
addNew texts = writeFile "cache.txt" $ show $ fmap hash texts

-- -- |Helper function for turning text into weather config.
-- readConfig :: Text -> SourcesConfig
-- readConfig = read . T.unpack

-- -- |Get configuration file from configs folder (configs/WeatherConfig)
-- -- |TODO allow user-defined file paths.
-- getConfig :: IO SourcesConfig
-- getConfig =
--     readConfig <$> (I.readFile "configs/SourcesConfig")
