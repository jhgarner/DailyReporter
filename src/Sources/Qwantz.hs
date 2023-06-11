module Sources.Qwantz (qwantz) where

import Sources.Lib

qwantz :: _ => Source es
qwantz = makeSource "Qwantz" do
  -- Unfortunately the title is only in the RSS feed...
  (title, summary) <- getTitleAndSummary $ https"www.qwantz.com"/:"rssfeed.php"
  usingHtml summary
  "title" `isEqualTo` title
