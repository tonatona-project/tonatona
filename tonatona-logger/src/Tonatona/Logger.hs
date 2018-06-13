{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tonatona.Logger
  -- ( run
  -- , Config(..)
  -- , Shared(..)
  -- , Tonatona.Db.init
  -- , TonaDbM
  -- , TonaDbConfig(..)
  -- , TonaDbShared(..)
  -- , runMigrate
  -- )
    where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT, reader)
import Tonatona (TonaM)
import Tonatona.Environment (TonaEnvConfig)
import qualified Tonatona.Environment as TonaEnv

-- instance MonadLogger (TonaM conf shared) where
--   monadLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> m ()

-- {-| Main function.
--  -}
-- run :: (TonaDbShared shared) => TonaDbM conf shared a -> TonaM conf shared a
-- run query = do
--   pool <- reader (dbPool . shared . snd)
--   runSqlPool query pool

-- runMigrate :: (TonaDbShared shared) => Migration -> TonaM conf shared ()
-- runMigrate migration = run $ runMigration migration

-- Shared

class TonaLoggerShared shared where
  shared :: shared -> Shared

data Shared = Shared
  { logger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  }

stdoutLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
stdoutLogger loc source level msg = do
  func <- runStdoutLoggingT $ LoggingT pure
  func loc source level msg
