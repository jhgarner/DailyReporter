{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.Text (Text, pack, intercalate, unpack)
import qualified Data.ByteString.Lazy.Char8 as B (unpack, pack)
-- import Data.Text as T
import qualified Data.Text.IO as I
import Network.AWS.S3
import Control.Monad.Trans.AWS
import Control.Lens
import Data.Conduit.Binary

import Parser

-- data SourcesConfig = Config {
--     sources :: [IO Text]
-- } deriving (Read)

-- |Prints email html compiled from `sources`
-- Html items are simply appended without additional formatting.
-- TODO: Allow user config for sources.
-- TODO: Allow easier creation for new sources of standard RSS type.
printEmailHtml :: IO String
printEmailHtml = do
  s <- sequence sources
  print "Done"
  -- filtered <- removeOld s
  -- addNew s
  pure $ unlines $ map unpack s 
  -- mapM_ I.putStrLn filtered

-- |Takes sources and compiles it into a single html. Then it emails it!
mailReport :: IO ()
mailReport = do
  env <- newEnv Discover
  s <- sequence sources
  filtered <- runResourceT . runAWST env $ removeOld s <* addNew s
  mail $ intercalate (pack "\n") filtered

-- |List of sources.
sources :: [IO Text]
sources = [print "weather" >> weather, print "ec" >> ec, print "apod" >> apod, print "smbd" >> smbc, print "butter" >> butter, print "pdl" >> pdl, print "qwantz" >> qwantz, print "word" >> word, print "xkcd" >> xkcd]

removeOld :: AWSConstraint r m => [Text] -> m [Text]
removeOld texts = do
  r <- send $ getObject "daily-reporter-cache" "cache"
  b <- view gorsBody r `sinkBody` sinkLbs
  return []
  let cache = read @[Int] $ B.unpack b
  return $
    if length cache /= length texts
      then texts
      else fmap snd $ filter (\(h, t) -> h /= hash t) $ zip cache texts

addNew :: AWSConstraint r m => [Text] -> m ()
addNew texts =
  void . send . putObject "daily-reporter-cache" "cache" . toBody . show $ fmap hash texts

-- -- |Helper function for turning text into weather config.
-- readConfig :: Text -> SourcesConfig
-- readConfig = read . T.unpack

-- -- |Get configuration file from configs folder (configs/WeatherConfig)
-- -- |TODO allow user-defined file paths.
-- getConfig :: IO SourcesConfig
-- getConfig =
--     readConfig <$> (I.readFile "configs/SourcesConfig")
