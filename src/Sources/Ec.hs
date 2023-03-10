{-# LANGUAGE OverloadedStrings #-}

module Sources.Ec (ec) where

import qualified Data.ByteString
import Data.Map (Map, insert)
import Data.Text (Text)
import Parser.HtmlParser (extractHtml)
import Utils (getTitleAndSummary)

ec :: IO (Map Text Text)
ec = do
  guide <- Data.ByteString.readFile "parsers/ec.json"
  (title, summary) <- getTitleAndSummary "https://existentialcomics.com/rss.xml"
  pure $ insert "*title" title $ extractHtml guide summary