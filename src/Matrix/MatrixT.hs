{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}

module Matrix.MatrixT (runRealMatrix, loginMatrix) where

import Config (RoomId (RoomId))
import Data.Aeson (decodeStrict')
import Fallible.Retryable (Retryable, orFallbackTo)
import Matrix.Class (Matrix (..))
import Network.Class
import Text.URI (URI (uriScheme), mkURI)
import Text.URI.QQ (scheme)
import Deriving.Aeson (UnwrapUnaryRecords)

-- Used to actually communicate with Matrix
runRealMatrix :: forall es. [Input LoginResponse, Network] :>> es => Eff (Matrix : es) ~> Eff es
runRealMatrix = interpret handler
  where
    handler :: forall esSend. (Handling esSend Matrix es) => Matrix (Eff esSend) ~> Eff es
    handler (UploadImage from) = do
      let Just uri = mkURI from
      let withScheme = uri {uriScheme = Just [scheme|https|]}
      let (url, _) = fromJust $ useHttpsURI withScheme
      image <- sendHandler @esSend $ Get url mempty
      let imageType = last $ splitOn "." from
      let contentType = header "Content-Type" [f|image/{imageType}|]
      auth <- getAuth
      response <- sendHandler @esSend $ Post (baseUrl /: "media" /: "v3" /: "upload") (ReqBodyBs image) (auth <> contentType)
      pure $ contentUri response
    handler (GetHash roomId key) = do
      auth <- getAuth
      response <- sendHandler @esSend $ Get (room roomId /: "state" /: hashEventType /: key) auth
      pure $ maybe "0" extractHash $ decodeStrict' response
    handler (PutHash roomId key toHash) = do
      auth <- getAuth
      sendHandler @esSend $ Put (room roomId /: "state" /: hashEventType /: key) (Hashes $ show $ hash toHash) auth
    handler (PutMsg roomId txnId "") = pure () -- Don't send empty messages
    handler (PutMsg roomId txnId contents) = do
      auth <- getAuth
      sendHandler @esSend $ Put (room roomId /: "send" /: "m.room.message" /: txnId) msg auth
      where
        msg =
          Msg
            { body = contents,
              format = "org.matrix.custom.html",
              formattedBody = contents,
              msgtype = "m.text"
            }

getAuth :: Input LoginResponse :> es => Eff es (Option Https)
getAuth = inputs $ oAuth2Bearer . encodeUtf8 . token . accessToken

loginMatrix ::
  (HasField "deviceId" config Text, HasField "password" config Text, [Retryable HttpException, Network, Input config] :>> es) =>
  Eff (Input LoginResponse : es) ~> Eff es
loginMatrix actions = do
  deviceId <- inputs $ getField @"deviceId"
  password <- inputs $ getField @"password"
  let body =
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
  loginResponse <- post (baseUrl /: "client" /: "v3" /: "login") (ReqBodyJson body) mempty `orFallbackTo` error "Bad"
  runInputConst loginResponse actions

room :: RoomId -> Url Https
room (RoomId roomId) = baseUrl /: "client" /: "v3" /: "rooms" /: roomId

baseUrl :: Url 'Https
baseUrl = https "matrix.org" /: "_matrix"

hashEventType :: Text
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

newtype SessionToken = SessionToken {token :: Text}
  deriving (Generic, Show)
  deriving (FromJSON) via CustomJSON '[UnwrapUnaryRecords] SessionToken