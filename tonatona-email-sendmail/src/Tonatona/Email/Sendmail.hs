module Tonatona.Email.Sendmail
  ( send
  , module Network.Mail.Mime
  ) where

import Control.Monad.IO.Class (liftIO)
import Network.Mail.Mime

import Tonatona (TonaM)

send :: Mail -> TonaM conf shared ()
send = liftIO . renderSendMail
