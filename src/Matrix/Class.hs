{-# LANGUAGE QuasiQuotes #-}

module Matrix.Class where

import Data.Text (Text)
import Config (RoomId)
import Data.Hashable (Hashable)
import PyF (fmt)
import Network.HTTP.Req (Req)
import Control.Monad.IO.Class (liftIO)

class Monad m => Matrix m where
  uploadImage :: Text -> m Text
  getHash :: RoomId -> Text -> m String
  putHash :: Hashable a => RoomId -> Text -> a -> m ()
  putMsg :: RoomId -> String -> Text -> m ()

-- Used for testing without writing to Matrix
instance Matrix Req where
  uploadImage = pure
  getHash _ key = pure "2"
  putHash _ key _ = pure ()
  putMsg _ _ body = liftIO $ putStrLn [fmt|Sending message:\n {body}\n=========\n\n|]
