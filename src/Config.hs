{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson (FromJSON, decode)
import Data.Text (Text)
import GHC.Exts (fromString)
import GHC.Generics (Generic)
import System.Environment (getEnv)
import Prelude hiding (readFile)

newtype RoomId = RoomId Text
  deriving (Generic, Show, FromJSON)

data Config = Config
  { apodApikey :: Text,
    deviceId :: Text,
    roomId :: RoomId,
    password :: Text,
    weatherApikey :: Text,
    long :: Text,
    lat :: Text
  }
  deriving (Show, Generic, FromJSON)

loadConfig :: IO Config
loadConfig = do
  file <- getEnv "CONFIG"
  let Just config = decode $ fromString file
  pure config
