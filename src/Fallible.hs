{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Fallible where

import Control.Monad.Catch (Handler, MonadMask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader)
import Control.Retry
  ( RetryStatus (RetryStatus, rsIterNumber),
    defaultLogMsg,
    fibonacciBackoff,
    limitRetries,
    logRetries,
    recovering,
  )
import Network.HTTP.Simple (HttpException)

allowFailureOf :: (MonadIO m, MonadMask m, Monoid a) => m a -> m a
allowFailureOf = runWithRetriesFallback mempty

runWithRetriesFallback :: (MonadIO m, MonadMask m) => a -> m a -> m a
runWithRetriesFallback fallback action = runWithRetries action (pure fallback)

orFallbackTo :: (MonadIO m, MonadMask m) => m a -> a -> m a
orFallbackTo = flip runWithRetriesFallback

detectFailuresOf :: (MonadIO m, MonadMask m) => m a -> m (Maybe a)
detectFailuresOf action = runWithRetries (fmap Just action) (pure Nothing)

runWithRetries :: (MonadIO m, MonadMask m) => m a -> m a -> m a
runWithRetries action onFailure =
  let policy = limitRetries 5 <> fibonacciBackoff 5000000
      runner _ RetryStatus {rsIterNumber = 4} = onFailure
      runner io _ = io
   in recovering policy [loggingHandler] $ runner action

loggingHandler :: forall m. MonadIO m => RetryStatus -> Handler m Bool
loggingHandler = logRetries (const @_ @HttpException $ pure True) (\crashed ex status -> liftIO $ print $ defaultLogMsg crashed ex status)