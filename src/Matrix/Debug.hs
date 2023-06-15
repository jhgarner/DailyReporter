{-# LANGUAGE QuasiQuotes #-}

module Matrix.Debug where

import Matrix.Class (Matrix(..))
import Logging.Info

-- Used for testing without writing to Matrix
runDebugMatrix :: InfoLog :> es => Interprets Matrix es
runDebugMatrix = interpret \case
  UploadImage url -> pure url
  GetHash _ key -> pure "1"
  PutHash _ key _ -> pure ()
  PutMsg _ body -> logInfo [f|Sending message:\n {body}\n=========\n\n|]
