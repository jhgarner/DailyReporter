{-# OPTIONS_GHC -Wno-missing-fields #-}

module SourcesSpec where

import Cleff
import Cleff.Input
import Control.Exception
import Data.Map.Strict (fromList, Map)
import Fallible.Throwing
import File.Class
import File.Filesystem (runWithFilesystem)
import Network.Class
import Network.HTTP.Req
import Sources.Xkcd
import Test.Hspec
import Control.Applicative (Applicative(liftA2))
import Data.Text
import Data.Aeson (decodeStrict')
import Control.Monad (forM_)
import Data.Foldable (traverse_)
import Sources.Weather
import Sources.Apod
import Sources.Ec
import Sources.Smbc
import Sources.Buttersafe
import Sources.PDL
import Sources.Qwantz
import Sources.Word
import Config (Config(apodApikey, weatherApikey, long, lat, Config))
import Sources.Sources (allSources)
import Sources.Lib
import Data.ByteString (readFile)
import Fallible.Retryable (Retryable(RunWithRetries))
import Fallible.Throwing (runThrowing)
import Cleff (toEff)

spec :: Spec
spec = traverse_ specSource allSources

specSource :: (Text, Eff [Input Config, Network, Retryable HttpException, File, IOE] SourceResult) -> Spec
specSource (name, action) = describe (unpack name) $
    it "parses correctly" $ checkSource action name

checkSource :: Eff [Input Config, Network, Retryable HttpException, File, IOE] SourceResult -> Text -> Expectation
checkSource action name = do
    let expectedFileName = "test/Sources/" <> name <> ".json"
    expectedJson <- Data.ByteString.readFile $ unpack expectedFileName
    actual <- runIOE $ runWithFilesystem $ fakeNetwork $ testConfig action
    let Just expected = decodeStrict' expectedJson
    actual `shouldBe` Right expected

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

testConfig :: Eff (Input Config:es) ~> Eff es
testConfig = interpret \Input -> pure Config{weatherApikey="key", long="10", lat="34", apodApikey="key"}
