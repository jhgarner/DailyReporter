{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Sources.Weather (weather) where

import Sources.Lib

weather :: _ => Source es
weather = makeSource "Weather" do
  Config{..} <- input
  usingJsonUrl (https"api.pirateweather.net"/:"forecast"/:weatherApikey/:[f|{long},{lat}|]) mempty
  "tht" `adjustedBy` getTime
  "tlt" `adjustedBy` getTime

getTime :: Text -> Text
getTime t = maybe t posixToTimeZone $ readMaybe t

posixToTimeZone :: Double -> Text
posixToTimeZone = formatClock . doubleToUtc

formatClock :: ZonedTime -> Text
formatClock = pack . formatTime defaultTimeLocale "%H:%M"

doubleToUtc :: Double -> ZonedTime
doubleToUtc = utcToZonedTime currentZone . posixSecondsToUTCTime . realToFrac

currentZone :: TimeZone
currentZone = hoursToTimeZone (-6)
