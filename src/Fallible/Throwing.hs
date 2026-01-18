module Fallible.Throwing (Throw, throw, runThrow) where

import Theseus.Effect.Error
import UnliftIO (Exception, catch, handle, throwIO)

rethrow :: Throw newError :> es => (error -> newError) -> Eff (Throw error : es) a -> Eff es a
rethrow f = interpret_ \(Throw e) -> throw $ f e
