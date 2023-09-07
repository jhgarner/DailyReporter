module SourcesSourcesSpec where
import Sources.Sources (isNewSource)
import Test.Hspec
import Logging.Info (InfoLog)
import Logging.Errors (ErrorLog)
import Fallible.Retryable
import Fallible.Throwing
import Matrix.Class
import Mock
import Sources.Lib
import Network.HTTP.Req

spec :: Spec
spec = do
    testIsNewSource

testIsNewSource :: Spec
testIsNewSource =
    describe "Checking if a source is new" do
        isNewSourceDuplicateCase
        isNewSourceNotDuplicateCase
        isNewSourceFailedToRun
        isNewIfMatrixDoesNotRespond

isNewSourceDuplicateCase = 
    it "the source is a duplicate" do
        result <- runWithMocks mocks $ isNewSource testingSource
        result `shouldBe` False
    where
        mocks = mock \(GetHashFromRoom x) -> pure $ hash testingParams

isNewSourceNotDuplicateCase = 
    it "the source is not a duplicate" do
        result <- runWithMocks mocks $ isNewSource testingSource
        result `shouldBe` True
    where
        mocks = mock \(GetHashFromRoom x) -> pure "old"

isNewSourceFailedToRun = 
    it "the source had an error" do
        result <- runWithMocks [] $ isNewSource failedSource
        result `shouldBe` True

isNewIfMatrixDoesNotRespond = 
    it "matrix didn't respond" do
        result <- runWithMocks mocks $ isNewSource failedSource
        result `shouldBe` True
    where
        mocks = mock \(GetHashFromRoom x) -> throw $ VanillaHttpException undefined

runWithMocks :: [Mock MatrixInRoom] -> _ -> _
runWithMocks mocked = runIOE . interpretMocked mocked . handleRetries @HttpException . dumpLogs

testingParams :: Params
testingParams = mempty

testingSource :: EvaluatedSource
testingSource = ("testing", Right testingParams)

failedSource :: EvaluatedSource
failedSource = ("testing", Left "Error")

dumpLogs :: Eff (InfoLog:ErrorLog:es) ~> Eff es
dumpLogs = ignoreOutput . ignoreOutput

handleRetries :: (Typeable e, Show e) => Eff (Retryable e:es) ~> Eff es
handleRetries = interpret \case
    RunWithRetries action fallback -> toEff (runThrowing action) >>= \case
        Left e -> toEff $ fallback e
        Right result -> pure result
