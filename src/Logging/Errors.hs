module Logging.Errors where
import Data.Text.IO (hPutStrLn)
import System.IO (stderr)

newtype ErrorText = ErrorText Text

type ErrorLog = Output ErrorText

logError :: ErrorLog :> es => Text -> Eff es ()
logError = output . ErrorText

printErrorStream :: IOE :> es => Eff (ErrorLog : es) ~> Eff es
printErrorStream = interpret \case
    Output (ErrorText message) -> liftIO $ hPutStrLn stderr message
