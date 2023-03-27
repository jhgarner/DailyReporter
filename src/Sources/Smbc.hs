module Sources.Smbc (smbc) where

import Parser.HtmlParser (extractHtml)
import Network.Class
import File.Class

smbc :: [Network, File, NetworkError] :>> es => Eff es (Map Text Text)
smbc = do
  guide <- getFile "parsers/smbc.json"
  (title, summary) <- getTitleAndSummary $ https "smbc-comics.com"/:"rss.php"
  pure $ insert "*title" title $ extractHtml guide summary