module Fresh.DateCounter (runFreshWithDate) where

import Data.Time.Clock

-- Generates a Fresh value including the current date
runFreshWithDate ::
  forall a es.
  IOE :> es =>
  Eff (Fresh String : es) ~> Eff es
runFreshWithDate =
  using runFreshMVar $ interpret_ \Fresh -> do
    UTCTime{utctDay} <- liftIO getCurrentTime
    counter <- fresh @Int
    pure [f|{fromEnum utctDay}day{counter}|]
