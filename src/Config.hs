{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config where

import Prelude hiding (readFile)
import Data.Text
import Data.ByteString
import Data.Text.IO
import Data.Aeson
import GHC.Generics
import System.Environment
import GHC.Exts (fromString)

data Person = Person
  { name :: Maybe Text,
    email :: Text
  }
  deriving (Show, Generic, FromJSON)

data Config = Config
  { fromName :: Maybe Text,
    fromEmail :: Text,
    to :: [Person],
    subject :: Text,
    accessKey :: Text,
    secretKey :: Text,
    apodApikey :: Text,
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
