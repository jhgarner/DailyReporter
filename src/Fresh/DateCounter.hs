module Fresh.DateCounter (runFreshWithDate) where

import Data.Time.Clock

-- Generates a Fresh value including the current date
runFreshWithDate ::
  forall a es.
  (IOE :> es) =>
  Eff (Fresh String : Fresh Int : es) ~> Eff es
runFreshWithDate =
  runFreshAtomicCounter . interpret \Fresh -> do
    UTCTime{utctDay} <- liftIO getCurrentTime
    counter <- fresh @Int
    pure [f|{fromEnum utctDay}day{counter}|]
