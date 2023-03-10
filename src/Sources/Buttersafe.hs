{-# LANGUAGE OverloadedStrings #-}

module Sources.Buttersafe (butter) where

import qualified Data.ByteString
import Data.Map (Map)
import Data.Text (Text)
import Parser.HtmlParser (extractHtml)
import Utils (getHttp)

butter :: IO (Map Text Text)
butter = do
  html <- getHttp "http://buttersafe.com"
  guide <- Data.ByteString.readFile "parsers/butter.json"
  pure $ extractHtml guide html