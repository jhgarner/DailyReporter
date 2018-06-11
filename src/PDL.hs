{-# LANGUAGE OverloadedStrings #-}

module PDL
    ( pdlHTML
    ) where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Text.Encoding
import Data.Text
import Text.HTML.Scalpel
import Utils
import Data.Map

pdlHTML :: IO Text
pdlHTML = do
  (title, _) <- getTitleAndSummary "http://feeds.feedburner.com/PoorlyDrawnLines?format=xml"
  img <- (scrapeImg . decodeUtf8 . B.toStrict) <$> simpleHttp "http://www.poorlydrawnlines.com/"
  return . template htmlS $ fromList [("*title", title), ("*img", img)]

htmlS =  pack "<center><h2>*title</h2></center><br><center>*img<br></center>"

scrapeImg :: Text -> Text
scrapeImg s = fromMaybe "" (scrapeStringLike s . innerHTML $ "div" @: [hasClass "post"])

removeBar :: Text -> Text
removeBar = replace "<tr xmlns=\"http://www.w3.org/1999/xhtml\"><td colspan=\"4\" align=\"center\"><a href=\"http://www.qwantz.com/archive.php\">archive</a> - <a href=\"mailto:ryan@qwantz.com?subject=sextuple%20is%20not%20as%20good%20as%20it%20sounds,%20especially%20when%20it%20comes%20to%20self-destructs\">contact</a> - <a href=\"http://www.topatoco.com/qwantz\">sexy exciting merchandise</a> - <a href=\"http://www.ohnorobot.com/index.pl?comic=23\">search</a> - <a href=\"http://www.qwantz.com/about.php\">about</a></td></tr>" ""
