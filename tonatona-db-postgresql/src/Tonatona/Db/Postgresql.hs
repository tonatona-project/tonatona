{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Tonatona.Db.Postgresql
  ( TonaDb.run
  , TonaDb.TonaDbM
  , Config(..)
  , DbConnStr(..)
  , DbConnNum(..)
  , TonaDb.TonaDbConfig(..)
  , Shared(..)
  , SharedSql
  , Tonatona.Db.Postgresql.init
  , TonaDb.TonaDbSqlShared(..)
  , runPostgres
  , TonaDb.runMigrate
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT)
import Data.Pool (Pool)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Tonatona.Db.Sql (Config(..), DbConnStr(..), DbConnNum(..), Shared(..), SharedSql, TonaDbConfig)
import qualified Tonatona.Db.Sql as TonaDb
import UnliftIO

genConnectionPool ::
     Config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO (Pool SqlBackend)
genConnectionPool (Config (DbConnStr connStr) (DbConnNum connNum)) logger = do
  let LoggingT runConnPool = createPostgresqlPool connStr connNum
  runConnPool logger

runPostgres ::
     MonadUnliftIO m
  => Pool SqlBackend
  -> ReaderT SqlBackend m a
  -> m a
runPostgres pool query = do
  runSqlPool query pool

-- TODO: Add function for freeing the pool.
init :: forall config.
     (TonaDbConfig config)
  => config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO SharedSql
init conf logger = do
  pool <- liftIO $ genConnectionPool (TonaDb.config conf) logger
  pure $ Shared $ \query -> runPostgres pool query
