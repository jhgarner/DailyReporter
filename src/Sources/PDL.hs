module Sources.PDL (pdl) where

import Sources.Lib

pdl :: _ => Source es
pdl = makeSource "PDL" do
  -- Unfortunately the title is only in the RSS feed...
  title <- getTitle $ https"feeds.feedburner.com"/:"PoorlyDrawnLines"
  usingHtmlUrl $ https"poorlydrawnlines.com"
  "title" `isEqualTo` title
