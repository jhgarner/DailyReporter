module DateCounterSpec where

import Config
import Data.List (nub)
import Fresh.DateCounter (runFreshWithDate)
import Matrix.Class
import Matrix.MatrixT (fakeLogin, runRealMatrix)
import Network.Class
import Test.Hspec
import Theseus.Effect.Error (runThrow)

spec :: Spec
spec = do
  describe "Fresh date" do
    it "is used to generate unique transactions for each Matrix message" do
      actual <- runIOE $ capturePuts $ runFreshWithDate $ fakeLogin $ runRealMatrix $ replicateM 10 do
        runThrow @HttpException $ putMsg (RoomId "room") "msg"
      actual `shouldSatisfy` eachIsUnique

eachIsUnique :: Eq a => [a] -> Bool
eachIsUnique ls = ls == nub ls

capturePuts :: Eff (Network : es) a -> Eff es [Url Https]
capturePuts action = fmap fst $ with action $ using @Network (runOutput @[Url Https]) $ interpret_ \case
  Put url _ _ -> output [url] $> error "The response from the Put is not be used by this test"
