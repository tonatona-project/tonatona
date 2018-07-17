{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Tonatona.Db.Postgresql
  ( run
  , TonaDbM
  , Config(..)
  , DbConnStr(..)
  , DbConnNum(..)
  , HasConfig(..)
  , Shared
  , Tonatona.Db.Postgresql.init
  , HasShared(..)
  , runMigrate
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (Loc, LoggingT(..), LogLevel, LogSource, LogStr)
import Control.Monad.Reader (ReaderT)
import Data.Pool (Pool)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (Migration, SqlBackend, runMigration, runSqlPool)
import Tonatona (TonaM, readerShared)
import Tonatona.Db (Config(..), DbConnStr(..), DbConnNum(..), HasConfig(..))
import UnliftIO (MonadUnliftIO)


type TonaDbM conf shared
  = ReaderT SqlBackend (TonaM conf shared)

genConnectionPool ::
     Config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO (Pool SqlBackend)
genConnectionPool (Config (DbConnStr connStr) (DbConnNum connNum)) logger = do
  let LoggingT runConnPool = createPostgresqlPool connStr connNum
  runConnPool logger

-- TODO: Add function for freeing the pool.
init :: forall config.
     HasConfig config
  => config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO Shared
init conf logger = do
  pool <- liftIO $ genConnectionPool (config conf) logger
  pure $ Shared pool

class HasShared shared where
  shared :: shared -> Shared

data Shared = Shared
  { connPool :: Pool SqlBackend
  }

runMigrate :: HasShared shared => Migration -> TonaM conf shared ()
runMigrate migration = run $ runMigration migration

-- | Main function.
run :: HasShared shared => TonaDbM conf shared a -> TonaM conf shared a
run query = do
  connPool <- readerShared (connPool . shared)
  runSqlPool query connPool
