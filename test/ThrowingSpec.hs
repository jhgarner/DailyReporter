module ThrowingSpec where

-- import Fallible.Throwing (runThrowing, throw)
import Test.Hspec

data Error = Error
  deriving (Show, Eq)

spec :: Spec
spec = pure ()

-- describe "Throwing effect" do
--     it "Doesn't execute code after the throw" do
--         let result = runPure @(Either () ()) $ runThrowing do
--                 throw ()
--                 error "This line should not run!"
--         result `shouldBe` Left ()
--     it "Doesn't touch successful operations" do
--         let result = runPure @(Either () ()) $ runThrowing $ pure ()
--         result `shouldBe` Right ()
