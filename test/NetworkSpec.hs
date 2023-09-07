module NetworkSpec where
import Test.Hspec
import Fallible.Throwing (throw, runThrowing)
import Network.Class
import Cleff
import Network.Network (runOnInternet)
import Network.HTTP.Req
import Data.ByteString
import Data.Either (isLeft)

data Error = Error
    deriving (Show, Eq)

spec :: Spec
spec = do
    describe "Network effect" do
        it "Doesn't leak exceptions" do
            actual <- runIOE $ runOnInternet $ runThrowing @HttpException $ get (https"notARealUrl") mempty
            actual `shouldSatisfy` isLeft
