{-# LANGUAGE OverloadedStrings #-}

module Email
    (mail
    ) where

-- Uses postfix sendmail to send it. So make sure it's configured.
import Network.Mail.Mime

-- Network.Mail.Mime uses lazy but everything else uses normal.
-- Some create packing and unpacking was necessary.
import Data.Text as T
import Data.Text.IO
import Data.Text.Lazy as L
import Utils

-- Used to get some basic information about sending mail.
data MailConfig = Config {
    from     :: (Maybe T.Text, T.Text),
    to       :: [(Maybe T.Text, T.Text)],
    subject  :: T.Text
} deriving (Read, Show)

-- |Sends mail with html. If the person cannot view html, it sends them the html
-- anyways!
mail :: T.Text -> IO ()
mail html = do
  m <- readConfig "MailConfig"
  renderSendMail . addPart [h, h] $ configToMail m
  where h = htmlPart $ L.fromStrict html

-- |Helper function to convert our user config at (configs/MailConfig) into a mailing.
configToMail :: MailConfig -> Mail
configToMail conf =
    Mail {
    mailFrom    = uncurry Address $ from conf,
    mailTo      = Prelude.map (uncurry Address) $ to conf,
    mailCc      = [],
    mailBcc     = [],
    mailHeaders = [("Subject", subject conf)],
    mailParts   = []
         }
