module Fallible.Throwing (Throw, throw, runThrow) where

import Theseus.Effect.Error
import UnliftIO (Exception, catch, handle, throwIO)

-- data Throw error :: Effect where
--     Throw :: error -> Throw error m a
-- -- makeEffect ''Throw
--
-- newtype ThrowingError error = ThrowingError { unwrapError :: error }
--
-- instance Show (ThrowingError error) where
--     show _ = "Error, ThrowingError shouldn't be capable of escaping!"
--
-- instance Typeable error => Exception (ThrowingError error) where
--
-- runThrowing :: (Typeable error) => Eff (Throw error : es) a -> Eff es (Either error a)
-- runThrowing pureAction = thisIsPureTrustMe $ catch impureAction makeLeft
--     where
--         impureAction = reinterpret onThrow $ Right <$> pureAction
--         onThrow (Throw error) = throwIO $ ThrowingError error
--         makeLeft = pure . Left . unwrapError

rethrow :: Throw newError :> es => (error -> newError) -> Eff (Throw error : es) a -> Eff es a
rethrow f = interpret_ \(Throw e) -> throw $ f e
