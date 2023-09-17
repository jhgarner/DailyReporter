{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Sources.Weather (weather) where

import Sources.Lib

weather :: _ => Source es
weather = makeSource "Weather" do
  Config{..} <- input
  Weather{..} <- getJson (https "api.pirateweather.net" /: "forecast" /: weatherApikey /: [f|{long},{lat}|]) mempty
  let Currently{..} = currently
  let Daily{..} = daily
  let Data{..} = head dailydata
  let pimt = getTime precipIntensityMaxTime
  let tht = getTime temperatureHighTime
  let tlt = getTime temperatureLowTime

  makeTitle (https "api.pirateweather.net") "Weather Report"
  makeText [f|Summary: {datasummary}|]
  makeText [f|Max Apparent Temperature: <b>{temperatureHigh}</b> at {tht}|]
  makeText [f|Max Apparent Temperature: <b>{temperatureLow}</b> at {tlt}|]
  makeText [f|Precipitation Intensity: <b>{precipIntensity}</b> inches per hour|]
  makeText [f|Max Intensity: <b>{precipIntensityMax}</b> at {pimt}|]
  makeText [f|Probability of Precipitation: <b>{precipProbability}</b>|]
  makeText [f|Currently: <b>{currentlysummary}</b>|]
  makeText [f|Current Temperature: <b>{apparentTemperature}</b>|]
  makeText [f|Rest of the week: <b>{dailysummary}</b>|]

data Weather = Weather
  { currently :: Currently
  , daily :: Daily
  }
  deriving (Generic, Show)
  deriving (FromJSON) via Vanilla Weather

data Currently = Currently
  { currentlysummary :: Text
  , apparentTemperature :: Double
  }
  deriving (Generic, Show)
  deriving (FromJSON) via Prefixed "currently" Currently

data Daily = Daily
  { dailysummary :: Text
  , dailydata :: [Data]
  }
  deriving (Generic, Show)
  deriving (FromJSON) via Prefixed "daily" Daily

data Data = Data
  { temperatureHigh :: Double
  , temperatureLow :: Double
  , temperatureHighTime :: Maybe Double
  , temperatureLowTime :: Maybe Double
  , precipProbability :: Double
  , precipIntensity :: Double
  , precipIntensityMax :: Double
  , precipIntensityMaxTime :: Maybe Double
  , datasummary :: Text
  }
  deriving (Generic, Show)
  deriving (FromJSON) via Prefixed "data" Data

getTime :: Maybe Double -> Text
getTime = maybe "Bad time" posixToTimeZone

posixToTimeZone :: Double -> Text
posixToTimeZone = formatClock . doubleToUtc

formatClock :: ZonedTime -> Text
formatClock = pack . formatTime defaultTimeLocale "%H:%M"

doubleToUtc :: Double -> ZonedTime
doubleToUtc = utcToZonedTime currentZone . posixSecondsToUTCTime . realToFrac

currentZone :: TimeZone
currentZone = hoursToTimeZone (-6)
