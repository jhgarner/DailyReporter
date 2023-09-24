module Sources.Exocomics (exocomics) where

import Sources.Lib

exocomics :: _ => Source es
exocomics = makeSource "Exocomics" do
  usingHtmlUrl home do
    getAttr "title" `on` comic >>= makeTitle home

    -- The Img url looks like /737/727.jpg which requires some processing
    imgPath <- getAttr "src" `on` comic
    makeImage $ renderUrl $ home /: imgPath

    quotes <- getHtmls quote
    for_ quotes makeText

home = https "exocomics.com"
comic = "img" `withClass` "image-style-main-comic"
quote = "p" `inside` ("li" `withClass` "comment-style-admin")
