{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tonatona.Db.Postgresql
  ( TonaDb.run
  , TonaDb.TonaDbM
  , Config(..)
  , DbConnStr(..)
  , DbConnNum(..)
  , TonaDb.TonaDbConfig(..)
  , Shared(..)
  , SharedSql(..)
  , Tonatona.Db.Postgresql.init
  , TonaDb.TonaDbSqlShared(..)
  , runPostgres
  , TonaDb.runMigrate
  ) where

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
import Tonatona.Db.Sql (Config(..), DbConnStr(..), DbConnNum(..), Shared(..), SharedSql, TonaDbConfig)
import qualified Tonatona.Db.Sql as TonaDb
import Tonatona.Environment (TonaEnvConfig)
import qualified Tonatona.Environment as TonaEnv
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
init :: forall config backend.
     (TonaDbConfig config)
  => config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO SharedSql
init conf logger = do
  pool <- liftIO $ genConnectionPool (TonaDb.config conf) logger
  pure $ Shared $ \query -> runPostgres pool query
