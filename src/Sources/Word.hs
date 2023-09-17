module Sources.Word (word) where

import Sources.Lib

word :: _ => Source es
word = makeSource "Word" do
  usingHtmlUrl home do
    getText `on` "h2" `withClass` "word-header-txt" >>= makeTitle home

    partOfSpeech <- getText `on` "span" `withClass` "main-attr"
    pronounciation <- getText `on` "span" `withClass` "word-syllables"
    makeText [f|{partOfSpeech} • {pronounciation}|]

    definitions <- getHtmls definition
    for_ definitions makeText

    getHtml example >>= makeText

    getHtml didYouKnow >>= makeText

definition = "p" `immediatelyInside` ("div" `withClass` "wod-definition-container")
example = "p" `immediatelyInside` ("div" `withClass` "left-content-box")
didYouKnow = "p" `inside` ("div" `withClass` "did-you-know-wrapper")
home = https "www.merriam-webster.com" /: "word-of-the-day"
