{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( runReport,
  )
where

import Config (Config (..), RoomId, loadConfig)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Reader (MonadIO (..), forM_)
import Data.Hashable (Hashable (hash))
import Data.Map (Map, traverseWithKey, (!?))
import Data.Text (Text)
import qualified Data.Text.IO
import Fallible
  ( runWithRetriesEmpty,
    runWithRetriesFallback,
    runWithRetriesMaybe,
  )
import Matrix.Class (Matrix (..))
import Matrix.MatrixT (runMatrix)
import PyF (fmt)
import Sources.Apod ( apod )
import Sources.Buttersafe ( butter )
import Sources.Ec ( ec )
import Sources.PDL ( pdl )
import Sources.Qwantz ( qwantz )
import Sources.Smbc ( smbc )
import Sources.Weather ( weather )
import Sources.Word ( word )
import Sources.Xkcd ( xkcd )
import Utils (ifM, template)
import Network.HTTP.Req (runReq, defaultHttpConfig)

runReport :: IO ()
runReport = do
  config@Config {..} <- loadConfig
  runReq defaultHttpConfig do
    runMatrix deviceId password do
      s <- sources config
      let combined = zip s [0 ..]
      forM_ combined $ \(text, id) -> runWithRetriesEmpty $ putMsg roomId (show id) text

sources :: (MonadIO m, MonadMask m, Matrix m) => Config -> m [Text]
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

runSource :: (MonadIO m, MonadMask m, Matrix m) => RoomId -> (Text, IO (Map Text Text)) -> m Text
runSource roomId (name, paramActions) = do
  liftIO $ putStrLn [fmt|Running {name}|]
  html <- liftIO $ Data.Text.IO.readFile [fmt|templates/{name}.html|]
  params <- runWithRetriesMaybe (liftIO paramActions)
  case params of
    Just params -> do
      ifM
        (isNewSection roomId (name, params))
        do
          runWithRetriesEmpty (writeCache roomId name params)
          replacedParams <- runWithRetriesFallback params (replaceImgs params)
          pure $ template html replacedParams
        do pure ""
    Nothing -> pure [fmt|<br/>Failed to generate a report for {name}<br/>|]

isNewSection :: Matrix m => RoomId -> (Text, Map Text Text) -> m Bool
isNewSection roomId (name, params) = do
  cached <- getHash roomId name
  pure $ cached /= show (hash params)

writeCache :: Matrix m => Hashable a => RoomId -> Text -> a -> m ()
writeCache roomId name value = putHash roomId name $ hash value

replaceImgs :: Matrix m => Map Text Text -> m (Map Text Text)
replaceImgs = traverseWithKey replaceImg
  where
    replaceImg "*img" url = uploadImage url
    replaceImg _ value = pure value