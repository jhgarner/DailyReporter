{-# LANGUAGE TemplateHaskell #-}

module Matrix.Class where

import Config
import Fallible.Throwing
import Network.Class

data Matrix :: Effect where
  UploadImage :: NetworkError :> effs => Text -> Matrix (Eff effs) Text
  GetHash :: NetworkError :> effs => RoomId -> Text -> Matrix (Eff effs) Int
  PutHash :: (NetworkError :> effs, Hashable a) => RoomId -> Text -> a -> Matrix (Eff effs) ()
  PutMsg :: NetworkError :> effs => RoomId -> Text -> Matrix (Eff effs) ()
makeEffect ''Matrix

data MatrixInRoom :: Effect where
  PutHashInRoom :: (NetworkError :> effs, Hashable a) => Text -> a -> MatrixInRoom (Eff effs) ()
  GetHashFromRoom :: NetworkError :> effs => Text -> MatrixInRoom (Eff effs) Int
  PutMsgInRoom :: NetworkError :> effs => Text -> MatrixInRoom (Eff effs) ()
makeEffect ''MatrixInRoom
