{-# LANGUAGE OverloadedStrings #-}

module Sources.Qwantz (qwantz) where

import qualified Data.ByteString
import Data.Map (Map, insert)
import Data.Text (Text)
import Parser.HtmlParser (extractHtml)
import Utils (getTitleAndSummary)

qwantz :: IO (Map Text Text)
qwantz = do
  guide <- Data.ByteString.readFile "parsers/qwantz.json"
  (title, summary) <- getTitleAndSummary "http://www.qwantz.com/rssfeed.php"
  -- Unfortunately the title is only in the RSS feed...
  pure $ insert "*title" title $ extractHtml guide summary