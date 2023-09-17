module Sources.Ec (ec) where

import Data.Text (intercalate)
import Sources.Lib

ec :: _ => Source es
ec = makeSource "Ec" $ do
  usingHtmlUrl home do
    getText title >>= makeTitle home

    -- Sometimes EC splits one comic into multiple images
    srcs <- getAttrs "src" `on` image
    for_ srcs makeImage

    -- But the title will just be on one of them
    titles <- getAttrs "title" `on` image
    makeText $ intercalate "\n" titles

title = "h3" `inside` ("div" `withClass` "title")
image = "img" `withClass` "comicImg"
home = https "existentialcomics.com"
