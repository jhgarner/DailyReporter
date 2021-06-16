{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Email
  ( mail,
  )
where

-- Network.Mail.Mime uses lazy but everything else uses normal.
-- Some create packing and unpacking was necessary.

import Config
import Data.Text as T
import Data.Text.Encoding as Te
import Data.Text.IO
import Data.Text.Lazy as L
import Network.Mail.Mime
import Network.Mail.Mime.SES

-- | Sends mail with html. If the person cannot view html, it sends them the html
--  anyways!
mail :: Config -> T.Text -> IO ()
mail config html = do
  renderSendMailSESGlobal (ses config) $ addPart [h, h] $ configToMail config
  where
    h = htmlPart $ L.fromStrict html
    ses Config {..} =
      SES
        { sesFrom = Te.encodeUtf8 fromEmail,
          sesTo = Te.encodeUtf8 . email <$> to,
          sesAccessKey = Te.encodeUtf8 accessKey,
          sesSecretKey = Te.encodeUtf8 secretKey,
          sesSessionToken = Nothing,
          sesRegion = usEast1
        }

-- | Helper function to convert our user config at (configs/MailConfig) into a mailing.
configToMail :: Config -> Mail
configToMail Config {..} =
  Mail
    { mailFrom = Address fromName fromEmail,
      mailTo = fmap person2Address to,
      mailCc = [],
      mailBcc = [],
      mailHeaders = [("Subject", subject)],
      mailParts = []
    }
  where
    person2Address Person {..} = Address name email
