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

import Data.Text
-- import Data.Text as T
import Data.Text.IO as I

-- data SourcesConfig = Config {
--     sources :: [IO Text]
-- } deriving (Read)

-- |Prints email html compiled from `sources`
-- Html items are simply appended without additional formatting.
-- TODO: Allow user config for sources.
-- TODO: Allow easier creation for new sources of standard RSS type.
printEmailHtml :: IO ()
printEmailHtml = do
  mapM_ (>>= I.putStrLn) sources

-- |Takes sources and compiles it into a single html. Then it emails it!
mailReport :: IO ()
mailReport = do
  s <- sequence sources
  mail $ intercalate (pack "\n") s

-- |List of sources.
sources :: [IO Text]
sources = [weather, ec, apod, smbc, butter, pdl, qwantz, word, xkcd]

-- -- |Helper function for turning text into weather config.
-- readConfig :: Text -> SourcesConfig
-- readConfig = read . T.unpack

-- -- |Get configuration file from configs folder (configs/WeatherConfig)
-- -- |TODO allow user-defined file paths.
-- getConfig :: IO SourcesConfig
-- getConfig =
--     readConfig <$> (I.readFile "configs/SourcesConfig")
