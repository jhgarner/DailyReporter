{-# LANGUAGE OverloadedStrings #-}

module PDL
    ( pdl
    ) where

import Data.Maybe
import Data.Text
import Text.HTML.Scalpel
import Utils

pdl :: IO Text
pdl = do
  (title, _) <- getTitleAndSummary "http://feeds.feedburner.com/PoorlyDrawnLines?format=xml"
  img <- scrapeImg <$> getHttp "http://www.poorlydrawnlines.com/"
  return $ template htmlS [("*title", title), ("*img", img)]

htmlS =  pack "<center><h2>*title</h2></center><br><center>*img<br></center>"

scrapeImg :: Text -> Text
scrapeImg s = fromMaybe "" . scrapeStringLike s . innerHTML $ "div" @: [hasClass "post"]
