{-# LANGUAGE QuasiQuotes #-}

module Matrix.Class where

import Config
import Fallible.Throwing
import Network.Class
import Tracing

data Matrix :: Effect where
  UploadImage :: NetworkError :> effs => Url Https -> Matrix (Eff effs) MyUrl
  GetHash :: NetworkError :> effs => RoomId -> Text -> Matrix (Eff effs) Text
  PutHash :: (NetworkError :> effs, Hashable a) => RoomId -> Text -> a -> Matrix (Eff effs) ()
  PutMsg :: NetworkError :> effs => RoomId -> Text -> Matrix (Eff effs) ()

uploadImage :: (Matrix :> effs, NetworkError :> effs) => Url Https -> Eff effs MyUrl
uploadImage url = send $ UploadImage url

getHash :: (Matrix :> effs, NetworkError :> effs) => RoomId -> Text -> Eff effs Text
getHash id key = send $ GetHash id key

putHash :: (Matrix :> effs, NetworkError :> effs, Hashable a) => RoomId -> Text -> a -> Eff effs ()
putHash id key a = send $ PutHash id key a

putMsg :: (Matrix :> effs, NetworkError :> effs) => RoomId -> Text -> Eff effs ()
putMsg id msg = send $ PutMsg id msg

instance Tracable Matrix where
  traceEffect (UploadImage _) = [f|UploadImage|]
  traceEffect (GetHash _ key) = [f|GetHash {key}|]
  traceEffect (PutHash _ key value) = [f|PutHash {key} {hash value}|]
  traceEffect (PutMsg _ msg) = [f|PutMsg {msg}|]

data MatrixInRoom :: Effect where
  PutHashInRoom :: (NetworkError :> effs, Hashable a) => Text -> a -> MatrixInRoom (Eff effs) ()
  GetHashFromRoom :: NetworkError :> effs => Text -> MatrixInRoom (Eff effs) Text
  PutMsgInRoom :: NetworkError :> effs => Text -> MatrixInRoom (Eff effs) ()

getHashFromRoom :: (MatrixInRoom :> effs, NetworkError :> effs) => Text -> Eff effs Text
getHashFromRoom key = send $ GetHashFromRoom key

putHashInRoom :: (MatrixInRoom :> effs, NetworkError :> effs, Hashable a) => Text -> a -> Eff effs ()
putHashInRoom key a = send $ PutHashInRoom key a

putMsgInRoom :: (MatrixInRoom :> effs, NetworkError :> effs) => Text -> Eff effs ()
putMsgInRoom msg = send $ PutMsgInRoom msg
