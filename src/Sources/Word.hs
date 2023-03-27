module Sources.Word (word) where

import Network.Class
import File.Class

word :: [Network, File, NetworkError] :>> es => Eff es (Map Text Text)
word = do
  (title, summary) <- getTitleAndSummary $ https "www.merriam-webster.com"/:"wotd"/:"feed"/:"rss2"
  pure $ fromList [("*title", title), ("*summary", decodeUtf8 summary)]