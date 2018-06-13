{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tonatona.Logger
  ( TonaLoggerShared(..)
  , Shared(..)
  , Tonatona.Logger.init
  , stdoutLogger
  , stderrLogger
  , logDebug
  , logInfo
  , logError
  , logWarn
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT, reader)
import Tonatona (TonaM)
import Tonatona.Environment (TonaEnvConfig)
import qualified Tonatona.Environment as TonaEnv

-- XXX: We could get rid of this overlapping instance by making TonaM be a
-- newtype wrapper instead of just a type alias.
instance {-# OVERLAPPING #-} TonaLoggerShared shared => MonadLogger (TonaM conf shared) where
  monadLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> TonaM conf shared ()
  monadLoggerLog loc source level msg = do
    let logstr = toLogStr msg
    logger <- reader (loggerAction . shared . snd)
    liftIO $ logger loc source level logstr

-- Shared

class TonaLoggerShared shared where
  shared :: shared -> Shared

data Shared = Shared
  { loggerAction :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  }

init :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> IO Shared
init logger = pure $ Shared logger

stdoutLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
stdoutLogger loc source level msg = do
  func <- runStdoutLoggingT $ LoggingT pure
  func loc source level msg

stderrLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
stderrLogger loc source level msg = do
  func <- runStderrLoggingT $ LoggingT pure
  func loc source level msg
