module Sources.Apod (apod) where

import Sources.Lib

--  |Astronomy Picture Of the Day. Maybe videos some days, which can never quite
--  be included into an email (iframes...)
apod :: _ => Source es
apod = makeSource "Apod" $ withCustomParserError "No picture today" do
  Config{apodApikey} <- input
  usingJsonUrl (https "api.nasa.gov"/:"planetary"/:"apod") ("api_key" =: apodApikey)
