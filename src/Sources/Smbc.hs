module Sources.Smbc (smbc) where

import Sources.Lib

smbc :: _ => Source es
smbc = makeSource "Smbc" do
  (title, summary) <- getTitleAndSummary feed
  makeTitle home title
  usingHtml summary do
    getAttr "src" `on` "img" >>= makeImage
    getHtml `on` "p" >>= makeText

home = https "smbc-comics.com"
feed = home /: "rss.php"
