{-# OPTIONS_GHC -Wno-missing-fields #-}

module SourcesSpec where

import Cleff
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
import Config (Config(weatherApikey, long, lat, Config))

spec :: Spec
spec = traverse_ specSource
    [ ("weather", weather Config{weatherApikey="key", long="10", lat="34"}),
      ("apod", apod "key"),
      ("xkcd", xkcd),
      ("ec", ec),
      ("smbc", smbc),
      ("butter", butter),
      ("pdl", pdl),
      ("qwantz", qwantz),
      ("word", word)
    ]

specSource :: (Text, Eff [Network, NetworkError, File, IOE] (Map Text Text)) -> Spec
specSource (name, action) = describe (unpack name) $
    it "parses correctly" $ checkSource action name

checkSource :: Eff [Network, NetworkError, File, IOE] (Map Text Text) -> Text -> Expectation
checkSource action name = do
    let expectedFileName = "test/Sources/" <> name <> ".json"
    (expectedJson, actual) <- runIOE $ runWithFilesystem $ liftA2 (,) (getFile expectedFileName) $ fakeNetwork action
    let Just expected = decodeStrict' expectedJson
    actual `shouldBe` expected

fakeNetwork :: File :> es => Eff (Network : NetworkError : es) ~> Eff es
fakeNetwork = handleError . handleNetwork
  where
    handleNetwork :: File :> es => Eff (Network : es) ~> Eff es
    handleNetwork = interpret \case
      Get url _ -> getFile $ "test/Sources/" <> renderUrl url <> ".html"
    handleError = interpret \case
      Throw e -> error "Something went very wrong!"
