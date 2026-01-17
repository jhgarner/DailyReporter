{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Sources.Sources where

import Data.Functor.Foldable (Corecursive (embed))
import Data.Text (intercalate, intersperse)
import Fallible.Retryable
import Logging.Context (withContext)
import Logging.Errors (logError)
import Logging.Info
import Matrix.Class
import Message
import Sources.Apod
import Sources.Buttersafe
import Sources.Ec
import Sources.Exocomics
import Sources.Lib
import Sources.Lib.SourceResult
import Sources.PDL
import Sources.Qwantz
import Sources.Smbc
import Sources.Weather
import Sources.Word
import Sources.Xkcd

sendAllSources :: _ => Eff es ()
sendAllSources = traverse_ sendSource allSources

allSources :: _ => [Source es]
allSources = [weather, apod, xkcd, ec, smbc, butter, pdl, qwantz, exocomics, word]

sendSource :: _ => Source _ -> Eff es ()
sendSource Source{..} = do
  withContext name do
    logInfo [f|Running Source|]
    handleFailureWith sourceErrorHandler $ evalSource name sourceAction
    logInfo [f|Source Ran|]

sourceErrorHandler :: _ => SourceError -> Eff es ()
sourceErrorHandler (SourceError error) = do
  logError [f|Source failed: {error}|]
  allowFailureOf $ putMsgInRoom error

evalSource :: _ => Text -> Eff es [Message] -> Eff es ()
evalSource hashKey action = do
  messages <- action
  ifM
    (isNewSource hashKey messages)
    do
      logInfo "Source was new"
      messagesWithValidImages <- traverse replaceImgs messages
      let message = intercalate "<br/>" $ fmap messageToText messagesWithValidImages
      writeNewParams hashKey messages `onSuccessOf` putMsgInRoom message
    do
      logInfo "Source was not new"

isNewSource :: _ => Text -> [Message] -> Eff es Bool
isNewSource hashKey messages = do
  logInfo [f|Checking if {hashKey} is new|]
  fallingBackTo True do
    cached <- getHashFromRoom hashKey
    pure $ cached /= hash messages

sendNewSource :: _ => Text -> [Message] -> Eff es ()
sendNewSource hashKey messages = do
  logInfo "Source was new"
  messagesWithValidImages <- traverse replaceImgs messages
  let message = intercalate "<br/>" $ fmap messageToText messagesWithValidImages
  writeNewParams hashKey messages `onSuccessOf` putMsgInRoom message

replaceImgs :: _ => Message -> Eff es Message
replaceImgs = cata \case
  ImgF (HttpsUrl url) -> Img <$> (uploadImage url `orFallbackTo` HttpsUrl url)
  message -> embed <$> sequence message

messageToText :: Message -> Text
messageToText = cata \case
  ImgF url -> [f|<img src="{getUrl url}"></img>|]
  HeaderF header -> [f|<h2>{header}</h2>|]
  ParagraphF paragraph -> [f|<p>{paragraph}</p>|]
  LinkF url content -> [f|<a href="{renderUrl url}">{content}</a>|]

writeNewParams :: _ => Text -> [Message] -> Eff es ()
writeNewParams hashKey messages = do
  logInfo [f|Writing new messages hash|]
  allowFailureOf $ putHashInRoom hashKey messages
