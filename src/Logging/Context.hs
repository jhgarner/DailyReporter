{-# LANGUAGE TemplateHaskell #-}

module Logging.Context where

import Cleff.Reader (Reader, asks, local, runReader)
import Data.Text (intercalate)

data LogContext m a where
  WithContext :: Text -> m a -> LogContext m a
  GetLoggingContext :: LogContext m Text
makeEffect ''LogContext

newtype Context = Context [Text]

addContext :: Text -> Context -> Context
addContext newContext (Context contexts) = Context $ newContext : contexts

getContext :: Context -> Text
getContext (Context contexts) = intercalate " | " contexts

runWithContext :: Eff (Reader Context : es) ~> Eff es
runWithContext = runReader (Context [])

runLogContext :: Eff (LogContext : es) ~> Eff es
runLogContext =
  runWithContext . reinterpret \case
    WithContext context action ->
      local (addContext context) $ toEff action
    GetLoggingContext -> asks getContext
