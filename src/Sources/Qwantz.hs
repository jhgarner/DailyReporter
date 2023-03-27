
module Sources.Qwantz (qwantz) where

import Parser.HtmlParser (extractHtml)
import Network.Class
import File.Class

qwantz :: [Network, File, NetworkError] :>> es => Eff es (Map Text Text)
qwantz = do
  guide <- getFile "parsers/qwantz.json"
  (title, summary) <- getTitleAndSummary $ https "www.qwantz.com"/:"rssfeed.php"
  -- Unfortunately the title is only in the RSS feed...
  pure $ insert "*title" title $ extractHtml guide summary