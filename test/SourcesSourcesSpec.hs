module SourcesSourcesSpec where

import Fallible.Retryable
import Fallible.Throwing
import Logging.Errors (ErrorLog)
import Logging.Info (InfoLog)
import Matrix.Class
import Mock
import Network.HTTP.Req
import Sources.Lib
import Sources.Sources (isNewSource)
import Test.Hspec

spec :: Spec
spec = do
  testIsNewSource

testIsNewSource :: Spec
testIsNewSource =
  describe "Checking if a source is new" do
    isNewSourceDuplicateCase
    isNewSourceNotDuplicateCase
    isNewIfMatrixDoesNotRespond

isNewSourceDuplicateCase =
  it "the source is a duplicate" do
    result <- runWithMocks mocks $ isNewSource "testing" []
    result `shouldBe` False
 where
  mocks = mock \(GetHashFromRoom "testing") -> pure $ hash testingParams

isNewSourceNotDuplicateCase =
  it "the source is not a duplicate" do
    result <- runWithMocks mocks $ isNewSource "testing" []
    result `shouldBe` True
 where
  mocks = mock \(GetHashFromRoom "testing") -> pure "old"

isNewIfMatrixDoesNotRespond =
  it "matrix didn't respond" do
    result <- runWithMocks mocks $ isNewSource "testing" []
    result `shouldBe` True
 where
  mocks = mock \(GetHashFromRoom x) -> throw $ VanillaHttpException undefined

runWithMocks :: [Mock MatrixInRoom] -> _ -> _
runWithMocks mocked = runIOE . interpretMocked mocked . handleRetries @HttpException . dumpLogs

testingParams :: [Message]
testingParams = mempty

-- failedSource :: EvaluatedSource
failedSource = ("testing", Left "Error")

dumpLogs :: Eff (InfoLog : ErrorLog : es) ~> Eff es
dumpLogs = ignoreOutput . ignoreOutput

handleRetries :: (Typeable e, Show e) => Eff (Retryable e : es) ~> Eff es
handleRetries = interpret \sender -> \case
  RunWithRetries action fallback ->
    pure $
      runThrow action >>= \case
        Left e -> fallback e
        Right result -> pure result
