module Sources.PDL (pdl) where

import Parser.HtmlParser (extractHtml)
import Network.Class
import File.Class

pdl :: [Network, File, NetworkError] :>> es => Eff es (Map Text Text)
pdl = do
  -- Unfortunately the title is only in the RSS feed...
  title <- getTitle $ https "feeds.feedburner.com"/:"PoorlyDrawnLines"
  guide <- getFile "parsers/pdl.json"
  html <- get (https "poorlydrawnlines.com") mempty
  pure $ insert "*title" title $ extractHtml guide html