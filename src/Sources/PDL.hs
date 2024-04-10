module Sources.PDL (pdl) where

import Sources.Lib

pdl :: _ => Source es
pdl = makeSource "PDL" do
  -- Unfortunately the title is only in the RSS feed...
  getTitle feed >>= makeTitle home
  usingHtmlUrl home do
    srcs <- getAttrs "href" `on` "a" `inside` ("div" `withClass` "entry-content")
    traverse makeImage srcs

home = https "poorlydrawnlines.com"
feed = https "feeds.feedburner.com" /: "PoorlyDrawnLines"
