module Logging.Info where

import Data.Text.IO (putStrLn)
import Logging.Context

newtype InfoText = InfoText Text

type InfoLog = Output InfoText

logInfo :: InfoLog :> es => Text -> Eff es ()
logInfo = output . InfoText

printInfoStream :: [IOE, LogContext] :>> es => Interprets InfoLog es
printInfoStream = interpret \case
  Output (InfoText message) -> do
    context <- getLoggingContext
    liftIO $ Data.Text.IO.putStrLn [f|[{context}] {message}|]
