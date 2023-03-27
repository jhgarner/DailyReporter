{-# LANGUAGE TemplateHaskell #-}

module Matrix.Class where

import Config (RoomId)
import Fallible.Throwing (Throw)
import Network.Class

data Matrix :: Effect where
  UploadImage :: NetworkError :> effs => Text -> Matrix (Eff effs) Text
  GetHash :: NetworkError :> effs => RoomId -> Text -> Matrix (Eff effs) String
  PutHash :: (NetworkError :> effs, Hashable a) => RoomId -> Text -> a -> Matrix (Eff effs) ()
  PutMsg :: NetworkError :> effs => RoomId -> Text -> Text -> Matrix (Eff effs) ()
makeEffect ''Matrix