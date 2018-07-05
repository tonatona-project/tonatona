module Tonatona.Email where

import Network.Mail.Mime (Mail)
import Tonatona (TonaM)

class TonaEmailShared shared where
  shared :: shared -> Shared

data Shared = Shared
  { sendEmail :: forall conf shared. Mail -> TonaM conf shared ()
  }

mkShared ::
     (forall conf shared. Mail -> TonaM conf shared ())
  -> Shared
mkShared = Shared
