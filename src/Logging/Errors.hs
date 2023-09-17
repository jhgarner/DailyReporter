module Logging.Errors where

import Data.Text.IO (hPutStrLn)
import Logging.Context (LogContext, getLoggingContext)
import System.IO (stderr)

newtype ErrorText = ErrorText Text

type ErrorLog = Output ErrorText

logError :: ErrorLog :> es => Text -> Eff es ()
logError = output . ErrorText

printErrorStream :: [IOE, LogContext] :>> es => Interprets ErrorLog es
printErrorStream = interpret \case
  Output (ErrorText message) -> do
    context <- getLoggingContext
    liftIO $ hPutStrLn stderr [f|[{context}] {message}|]
