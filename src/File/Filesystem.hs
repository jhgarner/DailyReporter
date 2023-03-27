module File.Filesystem where

import Data.ByteString (readFile)
import File.Class

runWithFilesystem :: IOE :> es => Eff (File : es) ~> Eff es
runWithFilesystem = interpret \case
    GetFile name -> liftIO $ Data.ByteString.readFile $ unpack name