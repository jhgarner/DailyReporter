module Network.Webdriver where

import Control.Monad.Logger (LoggingT, runStderrLoggingT)
import Test.WebDriver
import Test.WebDriver.Capabilities (ChromeOptions (_chromeOptionsArgs))
import Test.WebDriver.WD
import UnliftIO (bracket)
import UnliftIO.Concurrent (threadDelay)

data Webdriver :: Effect where
  Browse :: Url Https -> Webdriver (Eff effs) ByteString

browse :: Webdriver :> effs => Url Https -> Eff effs ByteString
browse url = send $ Browse url

runChrome :: IOE :> es => Eff (Webdriver : es) a -> Eff es a
runChrome action = with action $ interpret_ \case
  Browse url -> liftIO $ runStderrLoggingT do
    bracket start closeSession' \ctx ->
      runWD ctx $ do
        openPage $ unpack $ renderUrl url
        encodeUtf8 <$> getSource
 where
  start :: LoggingT IO Session
  start = do
    driver <- mkManualDriver "localhost" 9515 "" mempty
    startSession'
      driver
      defaultCaps
        { _capabilitiesGoogChromeOptions = Just defaultChromeOptions
        , _capabilitiesPageLoadStrategy = Just "eager"
        }
      "Reporter"
