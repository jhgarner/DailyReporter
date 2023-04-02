module ThrowingSpec where
import Test.Hspec
import Fallible.Throwing (throw, runThrowing)
import Cleff

data Error = Error
    deriving (Show, Eq)

spec :: Spec
spec = do
    describe "Throwing effect" do
        it "Doesn't leak exceptions" do
            runPure @(Either () ()) (runThrowing (throw ())) `shouldBe` Left ()