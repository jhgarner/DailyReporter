module Sources.Lib.SourceFactory where

import Data.Aeson.Decoding
import Fallible.Retryable
import Fallible.Throwing
import Message
import Network.Class
import Sources.Lib.Scraper
import Sources.Lib.SourceResult
import Text.HTML.TagSoup.Fast
import Text.URI
import Text.URI.QQ

data SourceFactory :: Effect where
  UsingHtmlUrl :: Throw SourceError :> es => Url 'Https -> Eff (ScraperEff : es) a -> SourceFactory (Eff es) a
  UsingHtml :: Throw SourceError :> es => ByteString -> Eff (ScraperEff : es) a -> SourceFactory (Eff es) a
  GetJson :: FromJSON a => Url 'Https -> Option 'Https -> SourceFactory m a

usingHtmlUrl :: (SourceFactory :> es, Throw SourceError :> es) => Url 'Https -> Eff (ScraperEff : es) a -> Eff es a
usingHtmlUrl url action = send $ UsingHtmlUrl url action

usingHtml :: (SourceFactory :> es, Throw SourceError :> es) => ByteString -> Eff (ScraperEff : es) a -> Eff es a
usingHtml url action = send $ UsingHtml url action

getJson :: (SourceFactory :> es, FromJSON a) => Url 'Https -> Option 'Https -> Eff es a
getJson url opt = send $ GetJson url opt

type SourceFactoryEnv es =
  Eff
    ( SourceFactory
        : NetworkError
        : MessageCollector
        : Throw SourceError
        : es
    )

runSourceFactory ::
  [Network, Retryable HttpException] :>> es =>
  Text ->
  SourceFactoryEnv es a ->
  Eff (Throw SourceError : es) [Message]
runSourceFactory name =
  collectMessages . handleNetworkErrors . interpret \sender -> \case
    UsingHtmlUrl url action -> do
      site <- get url mempty
      let tags = parseTagsT site
      pure $ runScraperEff tags action
    UsingHtml site action -> do
      pure $ runScraperEff (parseTagsT site) action
    GetJson url options -> do
      json <- get url options
      fmap pure $ either (throw . SourceError . pack) pure $ eitherDecodeStrict json
 where
  handleNetworkErrors = handleFailureWith $ throw . SourceError . pack . show
