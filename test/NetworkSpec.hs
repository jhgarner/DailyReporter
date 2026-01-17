module NetworkSpec where

import Data.ByteString
import Data.Either (isLeft)
import Fallible.Throwing (runThrow, throw)
import Network.Class
import Network.HTTP.Req
import Network.Network (runOnInternet)
import Test.Hspec

data Error = Error
  deriving (Show, Eq)

spec :: Spec
spec = do
  describe "Network effect" do
    it "Doesn't leak exceptions" do
      actual <- runIOE $ runOnInternet $ runThrow @HttpException $ get (https "notARealUrl") mempty
      actual `shouldSatisfy` isLeft
