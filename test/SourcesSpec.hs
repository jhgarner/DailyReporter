{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module SourcesSpec where

import Config (Config (Config, apodApikey, lat, long, weatherApikey))
import Control.Exception
import Control.Monad (forM_)
import Data.Aeson (decodeStrict', encode)
import Data.ByteString (readFile)
import Data.ByteString.Lazy qualified as B
import Data.Either (fromRight)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.IO qualified
import Fallible.Retryable
import Fallible.Throwing
import Network.Class
import Network.HTTP.Req
import Sources.Apod
import Sources.Buttersafe
import Sources.Ec
import Sources.Lib
import Sources.Lib.SourceResult
import Sources.PDL
import Sources.Qwantz
import Sources.Smbc
import Sources.Sources (allSources)
import Sources.Weather
import Sources.Word
import Sources.Xkcd
import Test.Hspec

spec :: Spec
spec = traverse_ specSource allSources

specSource :: Source [Input Config, Network, Retryable HttpException, IOE] -> Spec
specSource Source{..} =
  describe (unpack name) $
    it "parses correctly" $
      checkSource sourceAction name

checkSource :: Eff [Throw SourceError, Input Config, Network, Retryable HttpException, IOE] [Message] -> Text -> Expectation
checkSource action name = do
  let expectedFileName = "test/Sources/" <> name
  expected <- Data.ByteString.readFile $ unpack expectedFileName
  possiblyActual <- runIOE $ fakeNetwork $ testConfig $ runThrowing action
  let actual = either (error . show) id possiblyActual
  Data.Text.IO.putStrLn $ decodeUtf8 $ B.toStrict $ encode actual
  Just actual `shouldBe` decodeStrict' expected

fakeNetwork :: IOE :> es => Eff (Network : Retryable HttpException : es) ~> Eff es
fakeNetwork = handleRetries . handleNetwork
 where
  handleNetwork :: IOE :> es => Eff (Network : es) ~> Eff es
  handleNetwork = interpret \case
    Get url _ -> liftIO $ Data.ByteString.readFile $ unpack $ "test/Sources/" <> renderUrl url <> ".html"
  handleRetries = interpret \case
    RunWithRetries action _ -> flip fmap (toEff $ runThrowing action) \case
      Left e -> error $ show e
      Right result -> result

testConfig :: Eff (Input Config : es) ~> Eff es
testConfig = interpret \Input -> pure Config{weatherApikey = "key", long = "10", lat = "34", apodApikey = "key"}
