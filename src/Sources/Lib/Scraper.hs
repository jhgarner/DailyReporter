module Sources.Lib.Scraper where

import Fallible.Throwing
import Sources.Lib.SourceResult
import Text.HTML.Scalpel.Core
import Text.HTML.TagSoup

data ScraperEff m a where
  GetAttrs :: String -> Selector -> ScraperEff m [Text]
  GetAttr :: String -> Selector -> ScraperEff m Text
  GetHtmls :: Selector -> ScraperEff m [Text]
  GetHtml :: Selector -> ScraperEff m Text
  GetText :: Selector -> ScraperEff m Text

getAttrs :: ScraperEff :> es => String -> Selector -> Eff es [Text]
getAttrs a b = send $ GetAttrs a b
getAttr :: ScraperEff :> es => String -> Selector -> Eff es Text
getAttr a b = send $ GetAttr a b

getHtmls :: ScraperEff :> es => Selector -> Eff es [Text]
getHtmls a = send $ GetHtmls a

getHtml :: ScraperEff :> es => Selector -> Eff es Text
getHtml a = send $ GetHtml a

getText :: ScraperEff :> es => Selector -> Eff es Text
getText a = send $ GetText a

runScraperEff :: Throw SourceError :> es => [Tag Text] -> Eff (ScraperEff : es) ~> Eff es
runScraperEff tags = interpret_ \case
  GetAttrs name selector ->
    scrapeIt $ attrs name selector
  GetAttr name selector ->
    scrapeIt $ attr name selector
  GetHtmls selector ->
    scrapeIt $ htmls selector
  GetHtml selector ->
    scrapeIt $ html selector
  GetText selector ->
    scrapeIt $ text selector
 where
  scrapeIt :: Throw SourceError :> es => Scraper Text a -> Eff es a
  scrapeIt scraper = maybe (throw $ SourceError "Scraper failed") pure $ scrape scraper tags

infixl 4 `withClass`
withClass :: TagName -> String -> Selector
withClass tag className = tag @: [hasClass className]

infixl 4 `withId`
withId :: TagName -> String -> Selector
withId tag id = tag @: ["id" @= id]

infixl 3 `inside`
inside :: TagName -> Selector -> Selector
inside outerTag inner = inner // outerTag @: []

infixl 3 `immediatelyInside`
immediatelyInside :: TagName -> Selector -> Selector
immediatelyInside outerTag inner = inner // outerTag @: [] `atDepth` 1

infixr 2 `on`
on :: (a -> b) -> a -> b
on = ($)
