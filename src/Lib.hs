{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
import File.Filesystem
import Logging.Errors
import Logging.Info
import Matrix.Class
import Matrix.Debug
import Matrix.MatrixT
import Network.Class
import Network.Network
import Sources.Sources
import Tracing

runReport :: IO ()
runReport = runEff do
  config@Config {..} <- input
  usingActiveRoom roomId sendAllSources

runEff :: Eff _ a -> IO a
runEff =
  runIOE
    . loadConfig
    . printInfoStream
    . printErrorStream
    . runFreshAtomicCounter
    . runRetryableTimer isRecoverable
    . runWithFilesystem
    . runOnInternet
    -- Uncomment this line to us a fake matrix effect
    -- . runDebugMatrix
    -- Uncomment this line to login to matrix and run a real version
    . loginMatrix . runRealMatrix
