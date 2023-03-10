{-# LANGUAGE OverloadedStrings #-}

module Sources.PDL (pdl) where

import qualified Data.ByteString
import Data.Map (Map, insert)
import Data.Text (Text)
import Parser.HtmlParser (extractHtml)
import Utils (getHttp, getTitle)

pdl :: IO (Map Text Text)
pdl = do
  -- Unfortunately the title is only in the RSS feed...
  title <- getTitle "http://feeds.feedburner.com/PoorlyDrawnLines"
  guide <- Data.ByteString.readFile "parsers/pdl.json"
  html <- getHttp "https://poorlydrawnlines.com/"
  pure $ insert "*title" title $ extractHtml guide html