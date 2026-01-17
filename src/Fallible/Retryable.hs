{-# LANGUAGE QuasiQuotes #-}

module Fallible.Retryable where

import Control.Concurrent (threadDelay)
import Fallible.Throwing
import Logging.Errors
import Tracing (Tracable)

data Retryable e :: Effect where
  RunWithRetries :: Eff (Throw e : effs) a -> (e -> Eff effs a) -> Retryable e (Eff effs) a

runWithRetries :: Retryable e :> es => Eff (Throw e : es) a -> (e -> Eff es a) -> Eff es a
runWithRetries action onError = send $ RunWithRetries action onError

runRetryableTimer :: (Show e, Typeable e, _) => (e -> Bool) -> Interprets (Retryable e) es
runRetryableTimer isWorthRetrying = interpret \sender -> \case
  RunWithRetries action fallback -> pure $ cata @Natural runRetries 5
   where
    runRetries nextAttempt =
      runThrow action >>= \case
        Left error
          | isWorthRetrying error -> do
              sender @ErrorLog $ logError [f|Failed with error: {show error}|]
              case nextAttempt of
                Just nextAttempt -> do
                  sender @EIO $ liftIO $ threadDelay 5000000
                  nextAttempt
                Nothing -> fallback error
          | otherwise -> do
              sender @ErrorLog $ logError [f|Failed with error and not retrying: {show error}|]
              fallback error
        Right result -> pure result

handleFailureWith :: _ => (exception -> Eff es a) -> Eff (Throw exception : es) a -> Eff es a
handleFailureWith = flip runWithRetries

allowFailureOf :: (Monoid a, _) => Eff (Throw HttpException : es) a -> Eff es a
allowFailureOf = runWithRetriesFallback mempty

runWithRetriesFallback :: _ => a -> Eff (Throw HttpException : es) a -> Eff es a
runWithRetriesFallback fallback action = runWithRetries action (const $ pure fallback)

orFallbackTo :: _ => Eff (Throw HttpException : es) a -> a -> Eff es a
orFallbackTo action fallback = runWithRetries action (const $ pure fallback)

fallingBackTo :: _ => a -> Eff (Throw HttpException : es) a -> Eff es a
fallingBackTo fallback action = runWithRetries action (const $ pure fallback)

detectFailuresOf :: _ => Eff (Throw HttpException : es) a -> Eff es (Maybe a)
detectFailuresOf action = runWithRetries (fmap Just action) (const $ pure Nothing)

onSuccessOf :: _ => Eff es b -> Eff (Throw HttpException : es) a -> Eff es ()
onSuccessOf onSuccess fallible =
  detectFailuresOf fallible >>= \case
    Just _ -> void onSuccess
    _ -> pure ()
