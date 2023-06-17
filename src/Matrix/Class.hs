{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Matrix.Class where

import Config
import Fallible.Throwing
import Network.Class
import Tracing

data Matrix :: Effect where
  UploadImage :: NetworkError :> effs => Text -> Matrix (Eff effs) Text
  GetHash :: NetworkError :> effs => RoomId -> Text -> Matrix (Eff effs) Text
  PutHash :: (NetworkError :> effs, Hashable a) => RoomId -> Text -> a -> Matrix (Eff effs) ()
  PutMsg :: NetworkError :> effs => RoomId -> Text -> Matrix (Eff effs) ()
makeEffect ''Matrix

instance Tracable Matrix where
  traceEffect (UploadImage _) = [f|UploadImage|]
  traceEffect (GetHash _ key) = [f|GetHash {key}|]
  traceEffect (PutHash _ key value) = [f|PutHash {key} {hash value}|]
  traceEffect (PutMsg _ msg) = [f|PutMsg {msg}|]

data MatrixInRoom :: Effect where
  PutHashInRoom :: (NetworkError :> effs, Hashable a) => Text -> a -> MatrixInRoom (Eff effs) ()
  GetHashFromRoom :: NetworkError :> effs => Text -> MatrixInRoom (Eff effs) Text
  PutMsgInRoom :: NetworkError :> effs => Text -> MatrixInRoom (Eff effs) ()
makeEffect ''MatrixInRoom
