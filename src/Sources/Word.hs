module Sources.Word (word) where

import Sources.Lib

word :: _ => Source es
word = makeSource "Word" do
  (title, summary) <- getTitleAndSummary $ https"www.merriam-webster.com"/:"wotd"/:"feed"/:"rss2"
  "title" `isEqualTo` title
  "summary" `isEqualTo` decodeUtf8 summary
