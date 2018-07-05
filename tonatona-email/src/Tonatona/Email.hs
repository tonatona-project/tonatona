module Tonatona.Email where

import Network.Mail.Mime (Mail)

import Tonatona (TonaM, readerShared)

class TonaEmailShared shared where
  shared :: shared -> Shared

data Shared = Shared
  { sendEmail :: forall conf shared. Mail -> TonaM conf shared ()
  }

send :: TonaEmailShared shared => Mail -> TonaM conf shared ()
send mail = do
  sendingFunc <- readerShared (sendEmail . shared)
  sendingFunc mail

mkShared ::
     (forall conf shared. Mail -> TonaM conf shared ())
  -> Shared
mkShared = Shared
