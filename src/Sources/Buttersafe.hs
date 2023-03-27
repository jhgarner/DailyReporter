
module Sources.Buttersafe (butter) where

import Parser.HtmlParser (extractHtml)
import File.Class
import Network.Class

butter :: [Network, File, NetworkError] :>> es => Eff es (Map Text Text)
butter = do
  html <- get (https "buttersafe.com") mempty
  guide <- getFile "parsers/butter.json"
  pure $ extractHtml guide html