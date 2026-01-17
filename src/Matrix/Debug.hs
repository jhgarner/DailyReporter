{-# LANGUAGE QuasiQuotes #-}

module Matrix.Debug where

import Logging.Info
import Matrix.Class (Matrix (..))

-- Used for testing without writing to Matrix
runDebugMatrix :: InfoLog :> es => Interprets Matrix es
runDebugMatrix = interpret_ \case
  UploadImage url -> pure $ HttpsUrl url
  GetHash _ key -> pure "1"
  PutHash _ key _ -> pure ()
  PutMsg _ body -> logInfo [f|Sending message:\n {body}\n=========\n\n|]
