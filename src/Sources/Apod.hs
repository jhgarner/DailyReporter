{-# LANGUAGE RecordWildCards #-}

module Sources.Apod (apod) where

import Sources.Lib

--  |Astronomy Picture Of the Day. Maybe videos some days, which can never quite
--  be included into an email (iframes...)
apod :: _ => Source es
apod = makeSource "Apod" do
  Config{apodApikey} <- input
  Apod{..} <- getJson api ("api_key" =: apodApikey)
  makeTitle home title
  case mediaType of
    "image" -> makeImage url
    _ -> makeText "No picture today"
  makeText explanation

home = https "apod.nasa.gov" /: "apod" /: "astropix.html"
api = https "api.nasa.gov" /: "planetary" /: "apod"

data Apod = Apod
  { explanation :: Text
  , title :: Text
  , url :: Text
  , mediaType :: Text
  }
  deriving (Generic, Show)
  deriving (FromJSON) via Snake Apod
