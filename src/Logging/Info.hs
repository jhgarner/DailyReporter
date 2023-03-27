module Logging.Info where

import Data.Text.IO (putStrLn)

newtype InfoText = InfoText Text

type InfoLog = Output InfoText

logInfo :: InfoLog :> es => Text -> Eff es ()
logInfo = output . InfoText

printInfoStream :: IOE :> es => Eff (InfoLog : es) ~> Eff es
printInfoStream = interpret \case
    Output (InfoText message) -> liftIO $ Data.Text.IO.putStrLn message
