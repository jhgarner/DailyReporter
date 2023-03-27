{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Fallible.Retryable where

import Control.Concurrent (threadDelay)
import Fallible.Throwing
import Logging.Errors

data Retryable e :: Effect where
  RunWithRetries :: Eff (Throw e : effs) a -> Eff effs a -> Retryable e (Eff effs) a
makeEffect ''Retryable

runRetryableTimer :: ('[IOE, ErrorLog] :>> es, Show e, Typeable e) => (e -> Bool) -> Eff (Retryable e : es) ~> Eff es
runRetryableTimer isWorthRetrying = interpret \case
  RunWithRetries action other -> cata @Natural runRetries 5
    where
      runRetries Nothing = toEff other -- No more attempts remaining
      runRetries (Just nextAttempt) = toEff (runThrowing action) >>= \case
        Left error
          | isWorthRetrying error -> do
            output $ ErrorText [f|Failed with error: {show error}|]
            liftIO $ threadDelay 1000000
            nextAttempt
          | otherwise -> do
            output $ ErrorText [f|Failed with error and not retrying: {show error}|]
            toEff other
        Right result -> pure result

allowFailureOf :: (Retryable e :> es, Monoid a) => Eff (Throw e : es) a -> Eff es a
allowFailureOf = runWithRetriesFallback mempty

runWithRetriesFallback :: (Retryable e :> es) => a -> Eff (Throw e : es) a -> Eff es a
runWithRetriesFallback fallback action = runWithRetries action (pure fallback)

orFallbackTo :: Retryable e :> es => Eff (Throw e : es) a -> a -> Eff es a
orFallbackTo action fallback = runWithRetries action (pure fallback)

detectFailuresOf :: Retryable e :> es => Eff (Throw e : es) a -> Eff es (Maybe a)
detectFailuresOf action = runWithRetries (fmap Just action) (pure Nothing)