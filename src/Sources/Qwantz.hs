module Sources.Qwantz (qwantz) where

import Data.ByteString as B
import Sources.Lib

qwantz :: _ => Source es
qwantz = makeSource "Qwantz" do
  usingHtmlUrlMod home uncommentRssSpan do
    title <- getText `on` "span" `withClass` "rss-title"
    makeTitle home title
    imgUrl <- getAttr "src" `on` "img" `withClass` "comic"
    makeImage $ "https://www.qwantz.com/" <> imgUrl
    getAttr "title" `on` "img" `withClass` "comic" >>= makeText

home = https "www.qwantz.com"

uncommentRssSpan :: ByteString -> ByteString
uncommentRssSpan = encodeUtf8 . replace "/span> -->" "/span>" . replace "<!-- <span" "<span" . decodeUtf8
