{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Matrix.MatrixT (runMatrix) where

import Config (RoomId (RoomId))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Aeson (decodeStrict')
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Hashable (Hashable (hash))
import Data.Text (Text, splitOn, unpack)
import Data.Text.Encoding (encodeUtf8)
import Deriving.Aeson.Stock
import GHC.Generics (Generic)
import Matrix.Class (Matrix (..))
import Network.HTTP.Client.Conduit (RequestBody (RequestBodyBS))
import Network.HTTP.Req
import Network.HTTP.Simple (Request, getResponseBody, httpBS, httpJSON, parseRequestThrow_, setRequestBearerAuth, setRequestBody, setRequestBodyJSON, setRequestHeader)
import PyF (fmt)

newtype MatrixT m a = MatrixT {unMatrixT :: ReaderT LoginResponse m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader LoginResponse, MonadThrow, MonadCatch, MonadMask, MonadHttp)

-- Used to actually communicate with Matrix
instance MonadHttp m => Matrix (MatrixT m) where
  uploadImage from = do
    SessionToken token <- asks accessToken
    image <- getResponseBody <$> httpBS (parseRequestThrow_ $ unpack from)
    let imageType = last $ splitOn "." from
    let contentTypeHeader = header "Content-Type" [fmt|image/{imageType}|]
    let authHeader = oAuth2Bearer $ encodeUtf8 token
    response <- req POST (mkRequest "media/v3/upload") (ReqBodyBs image) jsonResponse $ authHeader <> contentTypeHeader
    pure $ contentUri $ responseBody response

  getHash roomId key = do
    token <- asks accessToken
    response <- get token [fmt|{room roomId}/state/{hashEventType}/{key}|]
    pure $ maybe "0" extractHash $ decodeStrict' response

  putHash roomId key toHash = do
    token <- asks accessToken
    put token [fmt|{room roomId}/state/{hashEventType}/{key}|] $ Hashes $ show $ hash toHash

  putMsg roomId txnId "" = pure () -- Don't send empty messages
  putMsg roomId txnId contents = do
    token <- asks accessToken
    put token [fmt|{room roomId}/send/m.room.message/{txnId}|] msg
    where
      msg =
        Msg
          { body = contents,
            format = "org.matrix.custom.html",
            formattedBody = contents,
            msgtype = "m.text"
          }

runMatrix :: MonadHttp m => Text -> Text -> MatrixT m a -> m a
runMatrix deviceId password (MatrixT action) = do
  loginResponse <- req POST (mkRequest "client/v3/login") (ReqBodyJson body) jsonResponse mempty
  runReaderT action $ responseBody loginResponse
  where
    body =
      LoginRequest
        { identifier =
            Identifier
              { idType = "m.id.user",
                user = "dailyreporterbot"
              },
          initialDeviceDisplayName = "bot",
          password = password,
          loginRequestDeviceId = deviceId,
          loginRequestType = "m.login.password"
        }

get :: MonadHttp m => SessionToken -> Text -> m ByteString
get (SessionToken token) target = do
  response <- req GET (mkRequest target) NoReqBody bsResponse (oAuth2Bearer (encodeUtf8 token))
  pure $ responseBody response

put :: (MonadHttp m, ToJSON body, FromJSON a, Show a) => SessionToken -> Text -> body -> m a
put (SessionToken token) target body = do
  response <- req PUT (mkRequest target) (ReqBodyJson body) jsonResponse (oAuth2Bearer (encodeUtf8 token))
  liftIO $ print response
  pure $ responseBody response

mkRequest :: Text -> Url Https
mkRequest path = foldl' (/:) baseUrl splitPath
  where
    baseUrl = https "matrix.org" /: "_matrix"
    splitPath = splitOn "/" path

room :: RoomId -> Text
room (RoomId roomId) = [fmt|client/v3/rooms/{roomId}|]

hashEventType :: String
hashEventType = "com.gmail.jkrmnj.hashes"

data LoginRequest = LoginRequest
  { identifier :: Identifier,
    initialDeviceDisplayName :: Text,
    password :: Text,
    loginRequestDeviceId :: Text,
    loginRequestType :: Text
  }
  deriving (Generic, Show)
  deriving (ToJSON) via PrefixedSnake "loginRequest" LoginRequest

data Identifier = Identifier
  { idType :: Text,
    user :: Text
  }
  deriving (Generic, Show)
  deriving (ToJSON) via PrefixedSnake "id" Identifier

newtype LoginResponse = LoginResponse
  {accessToken :: SessionToken}
  deriving (Generic, Show)
  deriving (FromJSON) via Snake LoginResponse

data Msg = Msg
  { body :: Text,
    format :: Text,
    formattedBody :: Text,
    msgtype :: Text
  }
  deriving (Generic, Show)
  deriving (ToJSON) via Snake Msg

newtype Hashes = Hashes {extractHash :: String}
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via Snake Hashes

newtype Content = Content {contentUri :: Text}
  deriving (Generic, Show)
  deriving (FromJSON) via Snake Content

newtype SessionToken = SessionToken Text
  deriving (Generic, Show)
  deriving (FromJSON) via Vanilla SessionToken