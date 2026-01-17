{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Message where

import Fallible.Throwing
import Sources.Lib.SourceResult (SourceError (SourceError))
import Text.URI
import Text.URI.QQ
import Theseus.Effect.State

data Message
  = Header Text
  | Paragraph Text
  | Img MyUrl
  | Link (Url Https) Message
  deriving (Generic, Eq, Show, FromJSON, ToJSON, Hashable)
makeBaseFunctor ''Message

data MessageCollector m a where
  MakeText :: Text -> MessageCollector m ()
  MakeImage :: Text -> MessageCollector m ()
  MakeTitle :: Url Https -> Text -> MessageCollector m ()
  MakeLink :: Url Https -> Text -> MessageCollector m ()

makeText :: MessageCollector :> es => Text -> Eff es ()
makeText msg = send $ MakeText msg

makeImage :: MessageCollector :> es => Text -> Eff es ()
makeImage img = send $ MakeImage img

makeTitle :: MessageCollector :> es => Url Https -> Text -> Eff es ()
makeTitle url title = send $ MakeTitle url title

makeLink :: MessageCollector :> es => Url Https -> Text -> Eff es ()
makeLink url txt = send $ MakeLink url txt

collectMessages :: Throw SourceError :> es => Eff (MessageCollector : es) a -> Eff es [Message]
collectMessages action =
  fmap getCollected $ with action $ using (execState (CollectedMessages [])) $ interpret_ \case
    MakeText message -> do
      modify $ addMessage $ Paragraph message
    MakeImage src -> do
      -- Normalize the URL to always have https at the start
      let encoded = replace " " "%20" src
      uri <- maybe (throw $ SourceError [f|bad url {src}|]) pure $ mkURI encoded
      let withScheme = uri{uriScheme = Just [scheme|https|]}
      -- Guaranteed to be valid because we set the scheme to https above
      let (url, _) = fromMaybe (error [f|url got janky {src}|]) $ useHttpsURI withScheme
      modify $ addMessage $ Link url (Img $ HttpsUrl url)
    MakeTitle link message -> do
      modify $ addMessage $ Link link (Header message)
    MakeLink link message -> do
      modify $ addMessage $ Link link (Paragraph message)

newtype CollectedMessages = CollectedMessages [Message]

addMessage :: Message -> CollectedMessages -> CollectedMessages
addMessage message (CollectedMessages messages) = CollectedMessages $ message : messages

getCollected :: CollectedMessages -> [Message]
getCollected (CollectedMessages messages) = reverse messages
