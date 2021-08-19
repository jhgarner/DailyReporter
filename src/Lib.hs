{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( mailReport,
  )
where

import Apod
import qualified Aws
import Aws.S3 as S3
import Buttersafe
import Config
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Retry
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as B (pack, unpack)
import Data.Conduit.Binary
import Data.Hashable
import Data.Text (Text, intercalate, pack, unpack)
import Data.Text.Encoding
import qualified Data.Text.IO as I
import Ec
import Email
import Network.HTTP.Conduit (HttpException (HttpExceptionRequest), RequestBody (..), newManager, responseBody, tlsManagerSettings)
import PDL
import Parser
import Qwantz
import Smbc
import Weather
import Word
import Xkcd

--  TODO: Allow user config for sources.
--  TODO: Allow easier creation for new sources of standard RSS type.

awsConfig = do
  Config {..} <- loadConfig
  creds <- Aws.makeCredentials (encodeUtf8 accessKey) (encodeUtf8 secretKey)
  pure
    Aws.Configuration
      { timeInfo = Aws.Timestamp,
        credentials = creds,
        logger = Aws.defaultLog Aws.Warning,
        proxy = Nothing
      }

runCommand r = do
  cfg <- awsConfig
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery
  Aws.simpleAws cfg s3cfg r

-- | Takes sources and compiles it into a single html. Then it emails it!
mailReport :: IO ()
mailReport = do
  config <- loadConfig
  s <- sequence $ sources config
  filtered <- removeOld s <* addNew s
  mail config $ intercalate (pack "\n") filtered

-- | List of sources.
sources :: Config -> [IO Text]
sources config =
  let policy = limitRetries 10 <> fibonacciBackoff 500000
      handler = const $
        Handler $ \case
          HttpExceptionRequest _ _ -> pure True
   in recovering policy [handler] . const
        <$> [ print "weather" >> weather config,
              print "ec" >> ec,
              print "apod" >> apod config,
              print "smbd" >> smbc,
              print "butter" >> butter,
              print "pdl" >> pdl uploadPdl,
              print "qwantz" >> qwantz,
              print "word" >> word,
              print "xkcd" >> xkcd
            ]

removeOld :: [Text] -> IO [Text]
removeOld texts = do
  S3.GetObjectMemoryResponse _ rsp <- runCommand $ getObject "daily-reporter-cache" "cache"
  let b = responseBody rsp
  let cache = read @[Int] $ B.unpack b
  return $
    if length cache /= length texts
      then texts
      else fmap snd $ filter (\(h, t) -> h /= hash t) $ zip cache texts

addNew :: [Text] -> IO ()
addNew texts =
  void . runCommand . putObject "daily-reporter-cache" "cache" . RequestBodyLBS . B.pack . show $ fmap hash texts

uploadPdl :: Text -> LB.ByteString -> IO Text
uploadPdl name content = do
  let url = "pdl" <> name <> ".png"
      object = putObject "daily-reporter-cache" url $ RequestBodyLBS content
  runCommand $ object {poAcl = Just AclPublicRead}
  pure url
