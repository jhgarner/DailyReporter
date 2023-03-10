{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Sources.Weather (weather) where

import Config (Config (..))
import Data.Map (Map, update)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
  ( TimeZone,
    hoursToTimeZone,
    utcToZonedTime,
  )
import Parser.JsonParser (extractJson)
import PyF (fmt)
import Text.Read (readMaybe)
import Utils (getHttp)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString
import Data.ByteString (ByteString)

weather :: Config -> IO (Map Text Text)
weather config = do
  w <- response config
  parser <- Data.ByteString.readFile "parsers/weather.json"
  let params1 = fromMaybe mempty $ extractJson parser w
  let params2 = update (Just . getTime) "*tht" params1
  let params = update (Just . getTime) "*tlt" params2
  pure params

-- | Grabs the json from the url for darksky api. Uses weather configuration.
response :: Config -> IO ByteString
response Config {..} = getHttp [fmt|https://api.pirateweather.net/forecast/{weatherApikey}/{long},{lat}|]

-- | Hardcoded TimeZone
currentZone :: TimeZone
currentZone = hoursToTimeZone (-6)

-- | Used to get time from darksky api. Solely for this.
--  TODO make this a little less gross to look at
getTime :: Text -> Text
getTime t =
  maybe
    t
    ( pack
        . formatTime defaultTimeLocale "%H:%M"
        . utcToZonedTime currentZone
        . posixSecondsToUTCTime
        . realToFrac
    )
    $ readMaybe @Double
    $ unpack t
