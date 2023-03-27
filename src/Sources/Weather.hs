{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Sources.Weather (weather) where

import Config (Config (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
  ( TimeZone,
    hoursToTimeZone,
    utcToZonedTime,
  )
import Parser.JsonParser (extractJson)
import Text.Read (readMaybe)
import Network.Class
import File.Class

weather :: [Network, File, NetworkError] :>> es => Config -> Eff es (Map Text Text)
weather Config{..} = do
  w <- get (https "api.pirateweather.net"/:"forecast"/:weatherApikey/:[f|{long},{lat}|]) mempty
  parser <- getFile "parsers/weather.json"
  let params1 = fromMaybe mempty $ extractJson parser w
  let params2 = update (Just . getTime) "*tht" params1
  let params = update (Just . getTime) "*tlt" params2
  pure params

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
