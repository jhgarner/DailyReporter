{-# LANGUAGE QuasiQuotes #-}

module Network.Class where

import Fallible.Throwing
import Network.HTTP.Req (HttpException (JsonHttpException))
import Text.Feed.Import (parseFeedString)
import Text.Feed.Query (feedItems, getItemSummary, getItemTitle)
import Text.Feed.Types (Item)
import Tracing (Tracable, traceEffect)

type NetworkError = Throw HttpException

data Network :: Effect where
  Get :: NetworkError :> effs => Url Https -> Option Https -> Network (Eff effs) ByteString
  Post :: (FromJSON result, HttpBody body, NetworkError :> effs) => Url Https -> body -> Option Https -> Network (Eff effs) result
  Put :: (FromJSON result, ToJSON body, NetworkError :> effs) => Url Https -> body -> Option Https -> Network (Eff effs) result

get :: (Network :> effs, NetworkError :> effs) => Url Https -> Option Https -> Eff effs ByteString
get url option = send $ Get url option

post :: (Network :> effs, FromJSON result, HttpBody body, NetworkError :> effs) => Url Https -> body -> Option Https -> Eff effs result
post url body option = send $ Post url body option

put :: (Network :> effs, FromJSON result, ToJSON body, NetworkError :> effs) => Url Https -> body -> Option Https -> Eff effs result
put url body option = send $ Put url body option

-- makeEffect ''Network

instance Tracable Network where
  traceEffect (Get url _) = [f|Get {show url}|]
  traceEffect (Post url _ _) = [f|Post {show url}|]
  traceEffect (Put url _ _) = [f|Post {show url}|]

isRecoverable :: HttpException -> Bool
isRecoverable (JsonHttpException _) = False
isRecoverable _ = True

-- | Gets the title and summary from XML given. Common in RSS feeds.
getTitleAndSummary :: [Network, NetworkError] :>> effs => Url 'Https -> Eff effs (Text, ByteString)
getTitleAndSummary name = do
  item <- getLatestItem . decodeUtf8 <$> get name mempty
  return $ fromMaybe mempty $ liftA2 (,) (getItemTitle item) (encodeUtf8 <$> getItemSummary item)

-- | Gets the title from an Rss feed's url
getTitle :: [Network, NetworkError] :>> effs => Url 'Https -> Eff effs Text
getTitle name = do
  item <- getLatestItem . decodeUtf8 <$> get name mempty
  return $ fromMaybe mempty $ getItemTitle item

-- | Returns latest item from RSS feed, but only if it's within the last 24 hours.
getLatestItem :: Text -> Item
getLatestItem xml =
  head $ feedItems $ fromJust $ parseFeedString (unpack xml) -- make it a string. Parse it.
