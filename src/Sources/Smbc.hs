module Sources.Smbc (smbc) where

import Sources.Lib

smbc :: _ => Source es
smbc = makeSource "Smbc" do
  (title, summary) <- getTitleAndSummary $ https"smbc-comics.com"/:"rss.php"
  usingHtml summary
  "title" `isEqualTo` title
