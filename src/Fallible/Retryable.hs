{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Fallible.Retryable where

import Control.Concurrent (threadDelay)
import Fallible.Throwing
import Logging.Errors
import Tracing (Tracable)

data Retryable e :: Effect where
  RunWithRetries :: Eff (Throw e : effs) a -> (e -> Eff effs a) -> Retryable e (Eff effs) a
makeEffect ''Retryable

runRetryableTimer :: (Show e, Typeable e, _) => (e -> Bool) -> Interprets (Retryable e) es
runRetryableTimer isWorthRetrying = interpret \case
  RunWithRetries action fallback -> cata @Natural runRetries 5
   where
    runRetries nextAttempt =
      toEff (runThrowing action) >>= \case
        Left error
          | isWorthRetrying error -> do
              logError [f|Failed with error: {show error}|]
              case nextAttempt of
                Just nextAttempt -> do
                  liftIO $ threadDelay 5000000
                  nextAttempt
                Nothing -> toEff $ fallback error
          | otherwise -> do
              logError [f|Failed with error and not retrying: {show error}|]
              toEff $ fallback error
        Right result -> pure result

handleFailureWith :: (_) => (HttpException -> Eff es a) -> Eff (Throw HttpException : es) a -> Eff es a
handleFailureWith = flip runWithRetries

allowFailureOf :: (Monoid a, _) => Eff (Throw HttpException : es) a -> Eff es a
allowFailureOf = runWithRetriesFallback mempty

runWithRetriesFallback :: (_) => a -> Eff (Throw HttpException : es) a -> Eff es a
runWithRetriesFallback fallback action = runWithRetries action (const $ pure fallback)

orFallbackTo :: (_) => Eff (Throw HttpException : es) a -> a -> Eff es a
orFallbackTo action fallback = runWithRetries action (const $ pure fallback)

fallingBackTo :: (_) => a -> Eff (Throw HttpException : es) a -> Eff es a
fallingBackTo fallback action = runWithRetries action (const $ pure fallback)

detectFailuresOf :: (_) => Eff (Throw HttpException : es) a -> Eff es (Maybe a)
detectFailuresOf action = runWithRetries (fmap Just action) (const $ pure Nothing)

onSuccessOf :: (_) => Eff es b -> Eff (Throw HttpException : es) a -> Eff es ()
onSuccessOf onSuccess fallible =
  detectFailuresOf fallible >>= \case
    Just _ -> void onSuccess
    _ -> pure ()
