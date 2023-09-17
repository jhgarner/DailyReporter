module Sources.Qwantz (qwantz) where

import Sources.Lib

qwantz :: _ => Source es
qwantz = makeSource "Qwantz" do
  -- Unfortunately the title is only in the RSS feed...
  (title, summary) <- getTitleAndSummary feed
  makeTitle home title
  usingHtml summary do
    getAttr "src" `on` "img" >>= makeImage
    getAttr "title" `on` "img" >>= makeText

home = https "www.qwantz.com"
feed = home /: "rssfeed.php"
