{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( runReport,
  )
where

import Config (Config (..), RoomId, loadConfig)
import Fallible.Retryable
  ( Retryable,
    allowFailureOf,
    detectFailuresOf,
    orFallbackTo,
    runRetryableTimer,
  )
import File.Class
import File.Filesystem
import Logging.Errors
import Logging.Info
import Matrix.Class
import Matrix.Debug (runDebugMatrix)
import Matrix.MatrixT
import Network.Class
import Network.Network
import Sources.Apod (apod)
import Sources.Buttersafe (butter)
import Sources.Ec (ec)
import Sources.PDL (pdl)
import Sources.Qwantz (qwantz)
import Sources.Smbc (smbc)
import Sources.Weather (weather)
import Sources.Word (word)
import Sources.Xkcd (xkcd)

runReport :: IO ()
runReport = runEff do
  config@Config {..} <- input
  s <- sources config
  let combined = zip s [0 ..]
  forM_ combined $ \(text, id) -> allowFailureOf $ putMsg roomId (pack $ show id) text

runEff :: Eff _ a -> IO a
runEff =
  runIOE
    . loadConfig
    . printInfoStream
    . printErrorStream
    . runRetryableTimer isRecoverable
    . runWithFilesystem
    . runOnInternet
    -- Uncomment this line to us a fake matrix effect
    -- . runDebugMatrix
    -- Uncomment this line to login to matrix and run a real version
    . loginMatrix . runRealMatrix

sources :: [Matrix, Network, File, Retryable HttpException, InfoLog] :>> es => Config -> Eff es [Text]
sources config@Config {..} =
  traverse
    (runSource roomId)
    [ ("Weather", weather config),
      ("Apod", apod apodApikey),
      ("Xkcd", xkcd),
      ("Ec", ec),
      ("Smbc", smbc),
      ("Buttersafe", butter),
      ("PDL", pdl),
      ("Qwantz", qwantz),
      ("Word", word)
    ]

runSource ::
  [Matrix, Network, File, Retryable HttpException, InfoLog] :>> es =>
  RoomId ->
  (Text, Eff (NetworkError : es) (Map Text Text)) ->
  Eff es Text
runSource roomId (name, paramActions) = do
  logInfo [f|Running {name}|]
  html <- decodeUtf8 <$> getFile [f|templates/{name}.html|]
  params <- detectFailuresOf paramActions
  case params of
    Just params -> do
      ifM
        (isNewSection roomId (name, params) `orFallbackTo` True)
        do
          allowFailureOf $ writeCache roomId name params
          replacedParams <- replaceImgs params `orFallbackTo` params
          pure $ template html replacedParams
        do pure ""
    Nothing -> pure [f|<br/>Failed to generate a report for {name}<br/>|]

template :: Text -> Map Text Text -> Text
template = ifoldr' replace

isNewSection :: [Matrix, NetworkError] :>> es => RoomId -> (Text, Map Text Text) -> Eff es Bool
isNewSection roomId (name, params) = do
  cached <- getHash roomId name
  pure $ cached /= show (hash params)

writeCache :: [Matrix, NetworkError] :>> es => Hashable a => RoomId -> Text -> a -> Eff es ()
writeCache roomId name value = putHash roomId name $ hash value

replaceImgs :: [Matrix, NetworkError] :>> es => Map Text Text -> Eff es (Map Text Text)
replaceImgs = itraverse replaceImg
  where
    replaceImg "*img" url = uploadImage url
    replaceImg _ value = pure value