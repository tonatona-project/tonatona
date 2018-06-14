{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tonatona.Db.Sql
  -- ( run
  -- , Config(..)
  -- , Shared(..)
  -- , Tonatona.Db.init
  -- , TonaDbM
  -- , TonaDbConfig(..)
  -- , TonaDbShared(..)
  -- , runMigrate
  -- ) where
  where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT, reader)
import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Semigroup ((<>))
import Data.String (IsString)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Migration, SqlBackend, runMigration, runSqlPool)
import System.Envy (FromEnv(..), Var, (.!=), env, envMaybe)
import Tonatona (TonaM)
import Tonatona.Environment (TonaEnvConfig)
import Tonatona.Db (TonaDbShared)
import Tonatona.Db as TonaDb
import qualified Tonatona.Environment as TonaEnv
import UnliftIO

type TonaDbM conf shared
  = ReaderT SqlBackend (TonaM conf shared)

type TonaDbSqlShared = TonaDbShared SqlBackend

type Shared = TonaDb.Shared SqlBackend

runMigrate :: (TonaDbSqlShared shared) => Migration -> TonaM conf shared ()
runMigrate migration = TonaDb.run $ runMigration migration

genConnectionPool ::
     Config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO (Pool SqlBackend)
genConnectionPool (Config (DbConnStr connStr) (DbConnNum connNum)) logger = do
  let LoggingT runConnPool = createPostgresqlPool connStr connNum
  runConnPool logger

runPostgres ::
     MonadUnliftIO m
  => Config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> ReaderT SqlBackend m a
  -> m a
runPostgres conf logger query = do
  pool <- liftIO $ genConnectionPool conf logger
  runSqlPool query pool
