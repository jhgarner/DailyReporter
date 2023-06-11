{-# LANGUAGE QuasiQuotes #-}

module Sources.Sources where
import Sources.Apod
import Sources.Buttersafe
import Sources.Ec
import Sources.Lib
import Sources.PDL
import Sources.Qwantz
import Sources.Smbc
import Sources.Weather
import Sources.Word
import Sources.Xkcd
import Fallible.Retryable
import Matrix.Class
import Logging.Info

sendAllSources :: _ => Eff es ()
sendAllSources = traverse_ sendSource allSources

allSources :: _ => [Source es]
allSources = [ weather, apod, xkcd, ec, smbc, butter, pdl, qwantz, word ]

sendSource :: _ => Source es -> Eff es ()
sendSource source = do
  logInfo [f|Running {nameOf source}|]
  source <- evaluate source
  whenM (isNewSource source) do
    message <- templateSource source
    writeNewParams source `onSuccessOf` putMsgInRoom message

isNewSource :: _ => EvaluatedSource -> Eff es Bool
isNewSource (name, Left _) = pure True -- Always display the error
isNewSource (name, Right params) = do
  logInfo [f|Checking if {name} is new|]
  isNewSection name params `orFallbackTo` True

isNewSection :: _ => Text -> Map Text Text -> Eff es Bool
isNewSection name params = do
  cached <- getHashFromRoom name
  pure $ cached /= hash params


templateSource :: _ => EvaluatedSource -> Eff es Text
templateSource (name, Left errorMessage) = pure errorMessage
templateSource (name, Right params) = do
  logInfo [f|Applying template to {name}|]
  html <- decodeUtf8 <$> getFile [f|templates/{name}.html|]
  paramsWithValidImages <- replaceImgs params `orFallbackTo` params
  pure (ifoldr' replace html paramsWithValidImages)

writeNewParams :: _ => EvaluatedSource -> Eff es ()
writeNewParams (name, Left _) = pure ()
writeNewParams (name, Right params) = allowFailureOf $ writeCache name params

writeCache :: _ => Hashable a => Text -> a -> Eff es ()
writeCache name value = putHashInRoom name $ hash value

replaceImgs :: _ => Map Text Text -> Eff es (Map Text Text)
replaceImgs = itraverse replaceImg
  where
    replaceImg "*img" url = uploadImage url
    replaceImg _ url = pure url
