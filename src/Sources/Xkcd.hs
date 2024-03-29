module Sources.Xkcd (xkcd) where

import Sources.Lib

-- | The final html text generated by scraping xkcd RSS feed.
xkcd :: _ => Source es
xkcd = makeSource "Xkcd" $ do
  usingHtmlUrl home do
    getAttr "alt" comic >>= makeTitle home
    getAttr "src" comic >>= makeImage
    getAttr "title" comic >>= makeText
  makeLink explain "Confused?"

explain = https "explainxkcd.com"
home = https "xkcd.com"
comic = "img" `inside` ("div" `withId` "comic")
