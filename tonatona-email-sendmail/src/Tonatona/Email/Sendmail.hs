module Tonatona.Email.Sendmail
  ( send
  , Tonatona.Email.Sendmail.init
  ) where

import Network.Mail.Mime (Mail, renderSendMail)
import Tonatona (TonaM)

send :: Mail -> TonaM conf shared ()
send mail = do
  sendingFunc <- readerShared sendEmail
  sendingFunc mail

init :: IO Shared
init = pure $ Shared (liftIO . renderSendMail)
