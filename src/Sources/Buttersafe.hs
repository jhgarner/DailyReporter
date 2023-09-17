module Sources.Buttersafe (butter) where

import Sources.Lib

butter :: _ => Source es
butter = makeSource "Buttersafe" do
  usingHtmlUrl home do
    getAttr "alt" `on` comic >>= makeTitle home
    getAttr "src" `on` comic >>= makeImage
    getHtml quote >>= makeText

home = https "buttersafe.com"
comic = "img" `inside` ("div" `withId` "comic")
quote = "div" `withClass` "entry"
