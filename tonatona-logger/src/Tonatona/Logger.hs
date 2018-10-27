{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tonatona.Logger
  ( HasShared(..)
  , Shared(..)
  , Tonatona.Logger.init
  , stdoutLogger
  , stderrLogger
  , noLogger
  , Logger.logDebug
  , Logger.logInfo
  , Logger.logError
  , Logger.logWarn
  ) where

import RIO

import Control.Monad.Logger as Logger
  ( Loc
  , LoggingT(..)
  , LogLevel
  , LogSource
  , LogStr
  , MonadLogger(..)
  , ToLogStr(toLogStr)
  , logDebug
  , logError
  , logInfo
  , logWarn
  , runStdoutLoggingT
  , runStderrLoggingT
  )
import Tonatona (TonaM)

-- XXX: We could get rid of this overlapping instance by making TonaM be a
-- newtype wrapper instead of just a type alias.
instance {-# OVERLAPPING #-} HasShared shared => MonadLogger (TonaM conf shared) where
  monadLoggerLog :: ToLogStr msg => Loc -> Logger.LogSource -> Logger.LogLevel -> msg -> TonaM conf shared ()
  monadLoggerLog loc source level msg = do
    let logstr = toLogStr msg
    logger <- asks (loggerAction . shared . snd)
    liftIO $ logger loc source level logstr

-- Shared

class HasShared shared where
  shared :: shared -> Shared

data Shared = Shared
  { loggerAction :: Loc -> Logger.LogSource -> Logger.LogLevel -> LogStr -> IO ()
  }

init :: (Loc -> Logger.LogSource -> Logger.LogLevel -> LogStr -> IO ()) -> IO Shared
init logger = pure $ Shared logger

stdoutLogger :: Loc -> Logger.LogSource -> Logger.LogLevel -> LogStr -> IO ()
stdoutLogger loc source level msg = do
  func <- runStdoutLoggingT $ LoggingT pure
  func loc source level msg

stderrLogger :: Loc -> Logger.LogSource -> Logger.LogLevel -> LogStr -> IO ()
stderrLogger loc source level msg = do
  func <- runStderrLoggingT $ LoggingT pure
  func loc source level msg

noLogger :: Loc -> Logger.LogSource -> Logger.LogLevel -> LogStr -> IO ()
noLogger _ _ _ _ = pure ()
