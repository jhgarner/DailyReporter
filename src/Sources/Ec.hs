module Sources.Ec (ec) where

import Parser.HtmlParser (extractHtml)
import File.Class
import Network.Class

ec :: [Network, File, NetworkError] :>> es => Eff es (Map Text Text)
ec = do
  html <- get (https "existentialcomics.com") mempty
  guide <- getFile "parsers/ec.json"
  pure $ extractHtml guide html