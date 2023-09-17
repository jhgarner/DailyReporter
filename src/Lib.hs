{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Lib (
  runReport,
)
where

import Config (Config (..), RoomId, loadConfig)
import Fallible.Retryable (
  Retryable,
  allowFailureOf,
  detectFailuresOf,
  orFallbackTo,
  runRetryableTimer,
 )
import Fresh.DateCounter
import Logging.Context
import Logging.Errors
import Logging.Info
import Matrix.Class
import Matrix.Debug
import Matrix.MatrixT
import Network.Class
import Network.Network
import Sources.Lib.SourceFactory
import Sources.Lib.SourceResult
import Sources.Sources
import Tracing

runReport :: IO ()
runReport = runEff do
  config@Config{..} <- input
  usingActiveRoom roomId sendAllSources

runEff :: Eff _ a -> IO a
runEff =
  runIOE
    . loadConfig
    . runLogContext
    . printInfoStream
    . printErrorStream
    . runFreshWithDate
    . runRetryableTimer isRecoverable
    . runRetryableTimer @SourceError (const False)
    . runOnInternet
    -- Uncomment this line to us a fake matrix effect
    -- . runDebugMatrix
    -- Uncomment this line to login to matrix and run a real version
    . loginMatrix
    . runRealMatrix
