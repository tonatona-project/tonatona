module Tonatona.Email.Sendmail
  ( send
  , Tonatona.Email.Sendmail.init
  , TonaEmailShared(..)
  , Shared(..)
  , module Network.Mail.Mime
  -- , NetworkMail.simpleMail'
  ) where

import Control.Monad.IO.Class (liftIO)
import Network.Mail.Mime

import Tonatona.Email (Shared(..), TonaEmailShared(..), send)

init :: IO Shared
init = pure $ Shared (liftIO . renderSendMail)
