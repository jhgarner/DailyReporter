{-# LANGUAGE OverloadedStrings #-}

module Qwantz
  ( qwantz
  ) where

import Data.Maybe
import Data.Text
import Data.Text.IO
import Text.HTML.Scalpel
import Utils
import Parser
import Control.Lens
import Data.HashMap.Strict (HashMap, toList)

-- |The final html text generated by the qwantz feed.
-- We then proceed to scrape only the image and title.
qwantz :: IO Text
qwantz = do
  h <- htmlS
  guide <- Data.Text.IO.readFile "parsers/qwantz.json"
  (title, summary) <- getTitleAndSummary "http://www.qwantz.com/rssfeed.php"
  -- Unfortunately the title is only in the RSS feed...
  pure (template (("*title", title) : toList (scrapeAlt guide summary)) h)


-- |Get template for qwantz (templates/Qwantz.html)
htmlS :: IO Text
htmlS = Data.Text.IO.readFile "templates/Qwantz.html"

-- |Scrape alt-text from qwantz
scrapeAlt :: Text -> Text -> HashMap Text Text
scrapeAlt guide s = fromMaybe mempty $ extractHtml guide s