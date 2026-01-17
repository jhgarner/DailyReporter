module Logging.Context where

import Control.Monad.Identity
import Data.Text (intercalate)
import Theseus.Effect.Reader (Reader, asks, local, runReader)

data LogContext m a where
  WithContext :: Text -> Eff (LogContext : es) a -> LogContext (Eff es) a
  GetLoggingContext :: LogContext m Text

withContext :: LogContext :> es => Text -> Eff (LogContext : es) a -> Eff es a
withContext ctx action = send $ WithContext ctx action

getLoggingContext :: LogContext :> es => Eff es Text
getLoggingContext = send GetLoggingContext

newtype Context = Context [Text]

addContext :: Text -> Context -> Context
addContext newContext (Context contexts) = Context $ newContext : contexts

getContext :: Context -> Text
getContext (Context contexts) = intercalate " | " contexts

runLogContext :: Eff (LogContext : es) ~> Eff es
runLogContext = go $ Context []
 where
  go :: Context -> Eff (LogContext : es) ~> Eff es
  go ctx =
    interpret \sender -> \case
      WithContext context action ->
        pure $ go (addContext context ctx) action
      GetLoggingContext -> pure $ pure $ getContext ctx
