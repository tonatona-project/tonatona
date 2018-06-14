{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tonatona.Db.Postgresql
  ( TonaDb.run
  , TonaDbM
  , Config(..)
  , DbConnStr(..)
  , DbConnNum(..)
  , TonaDb.TonaDbConfig(..)
  , Shared
  , init
  , TonaDbSqlShared(..)
  , runMigrate
  , runPostgres
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
import Tonatona.Db.Sql (Config(..), DbConnStr(..), DbConnNum(..), TonaDbShared)
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
  => Config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> ReaderT SqlBackend m a
  -> m a
runPostgres conf logger query = do
  pool <- liftIO $ genConnectionPool conf logger
  runSqlPool query pool

init :: forall config backend.
     (TonaDbConfig config)
  => config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO (Shared backend)
init conf logger = TonaDb.init conf logger runPostgres
