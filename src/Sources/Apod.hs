module Sources.Apod (apod) where

import Parser.JsonParser (extractJson)
import Network.Class
import File.Class

--  |Astronomy Picture Of the Day. Maybe videos some days, which can never quite
--  be included into an email (iframes...)
apod :: [Network, File, NetworkError] :>> es => Text -> Eff es (Map Text Text)
apod apodApikey = do
  json <- get (https "api.nasa.gov"/:"planetary"/:"apod") ("api_key" =: apodApikey)
  parser <- getFile "parsers/apod.json"
  pure $ fromMaybe (singleton "*title" "no picture today") $ extractJson parser json