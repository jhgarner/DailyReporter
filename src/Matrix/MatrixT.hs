{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}

module Matrix.MatrixT (runRealMatrix, loginMatrix, usingActiveRoom) where

import Config (Config (deviceId), RoomId (RoomId))
import Config qualified
import Data.Aeson (decodeStrict')
import Deriving.Aeson (UnwrapUnaryRecords)
import Fallible.Retryable (Retryable, orFallbackTo)
import Matrix.Class
import Network.Class
import Text.URI (URI (uriScheme), mkURI)
import Text.URI.QQ (scheme)

type In = (:>)
type AllIn effects stack = effects :>> stack

-- Used to actually communicate with Matrix
runRealMatrix :: forall es. [Input LoginResponse, Fresh String, Network] `AllIn` es => Interprets Matrix es
runRealMatrix action = do
  auth <- getAuth
  txnId <- tshow <$> fresh @String
  with action $ interpret \sender ->
    pure . \case
      PutHash roomId key toHash -> do
        sender @Network $ put (roomUrl /~ roomId /: "state" /: hashEventType /: key) (Hashes $ hash toHash) auth
      GetHash roomId key -> do
        response <- sender @Network $ get (roomUrl /~ roomId /: "state" /: hashEventType /: key) auth
        pure $ maybe "0" extractHash $ decodeStrict' response
      PutMsg roomId contents -> do
        sender @Network $ put (roomUrl /~ roomId /: "send" /: "m.room.message" /: txnId) msg auth
       where
        msg =
          Msg
            { body = contents
            , format = "org.matrix.custom.html"
            , formattedBody = contents
            , msgtype = "m.text"
            }
      UploadImage url -> do
        image <- sender @Network $ get url mempty
        let imageType = last $ splitOn "." $ renderUrl url
        let contentType = header "Content-Type" [f|image/{imageType}|]
        response <- sender @Network $ post (baseUrl /: "media" /: "v3" /: "upload") (ReqBodyBs image) (auth <> contentType)
        pure $ MxcUrl $ contentUri response
 where
  getAuth :: _ => Eff es (Option Https)
  getAuth = inputs $ oAuth2Bearer . encodeUtf8 . token . accessToken

  roomUrl :: Url Https
  roomUrl = baseUrl /: "client" /: "v3" /: "rooms"

  hashEventType :: Text
  hashEventType = "com.gmail.jkrmnj.hashes"

loginMatrix :: _ => Eff (Input LoginResponse : es) a -> Eff es a
loginMatrix actions = do
  deviceId <- inputs deviceId
  password <- inputs Config.password
  let body =
        LoginRequest
          { identifier =
              Identifier
                { idType = "m.id.user"
                , user = "dailyreporterbot"
                }
          , initialDeviceDisplayName = "bot"
          , password = password
          , loginRequestDeviceId = deviceId
          , loginRequestType = "m.login.password"
          }
  loginResponse <- post (baseUrl /: "client" /: "v3" /: "login") (ReqBodyJson body) mempty `orFallbackTo` error "Bad"
  runInput loginResponse actions

baseUrl :: Url 'Https
baseUrl = https "matrix.org" /: "_matrix"

data LoginRequest = LoginRequest
  { identifier :: Identifier
  , initialDeviceDisplayName :: Text
  , password :: Text
  , loginRequestDeviceId :: Text
  , loginRequestType :: Text
  }
  deriving (Generic, Show)
  deriving (ToJSON) via PrefixedSnake "loginRequest" LoginRequest

data Identifier = Identifier
  { idType :: Text
  , user :: Text
  }
  deriving (Generic, Show)
  deriving (ToJSON) via PrefixedSnake "id" Identifier

newtype LoginResponse = LoginResponse
  {accessToken :: SessionToken}
  deriving (Generic, Show)
  deriving (FromJSON) via Snake LoginResponse

data Msg = Msg
  { body :: Text
  , format :: Text
  , formattedBody :: Text
  , msgtype :: Text
  }
  deriving (Generic, Show)
  deriving (ToJSON) via Snake Msg

newtype Hashes = Hashes {extractHash :: Text}
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via Snake Hashes

newtype Content = Content {contentUri :: Text}
  deriving (Generic, Show)
  deriving (FromJSON) via Snake Content

newtype SessionToken = SessionToken {token :: Text}
  deriving (Generic, Show)
  deriving (FromJSON) via CustomJSON '[UnwrapUnaryRecords] SessionToken

usingActiveRoom :: forall es. Matrix :> es => RoomId -> Interprets MatrixInRoom es
usingActiveRoom roomId = interpret \sender ->
  pure . \case
    GetHashFromRoom key -> sender @Matrix $ getHash roomId key
    PutHashInRoom key hash -> sender @Matrix $ putHash roomId key hash
    PutMsgInRoom msg -> sender @Matrix $ putMsg roomId msg
