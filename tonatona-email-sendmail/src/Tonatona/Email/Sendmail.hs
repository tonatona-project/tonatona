module Tonatona.Email.Sendmail
  ( send
  , module Network.Mail.Mime
  ) where

import RIO

import Network.Mail.Mime

send :: Mail -> RIO env ()
send = liftIO . renderSendMail
