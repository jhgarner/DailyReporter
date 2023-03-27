{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson (FromJSON, decode)
import Data.Text (Text)
import GHC.Exts (fromString)
import GHC.Generics (Generic)
import System.Environment (getEnv)

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

loadConfig :: IOE :> es => Eff (Input Config : es) a -> Eff es a
loadConfig = interpret \Input -> do
  file <- liftIO $ getEnv "CONFIG"
  let Just config = decode $ fromString file
  pure config
