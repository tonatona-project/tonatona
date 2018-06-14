{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tonatona.Db.Sqlite
  ( TonaDb.run
  , TonaDb.TonaDbM
  , Config(..)
  , DbConnStr(..)
  , DbConnNum(..)
  , TonaDb.TonaDbConfig(..)
  , Shared
  , Tonatona.Db.Sqlite.init
  , TonaDb.TonaDbSqlShared(..)
  , runSqlite
  , TonaDb.runMigrate
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT, reader)
import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Semigroup ((<>))
import Data.String (IsString)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Sqlite (createSqlitePool)
import Database.Persist.Sql (ConnectionPool, Migration, SqlBackend, runMigration, runSqlPool)
import System.Envy (FromEnv(..), Var, (.!=), env, envMaybe)
import Tonatona (TonaM)
import Tonatona.Db.Sql (Config(..), DbConnStr(..), DbConnNum(..), Shared, TonaDbConfig)
import qualified Tonatona.Db.Sql as TonaDb
import Tonatona.Environment (TonaEnvConfig)
import qualified Tonatona.Environment as TonaEnv
import UnliftIO

genConnectionPool ::
     Config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO (Pool SqlBackend)
genConnectionPool (Config (DbConnStr connStr) (DbConnNum connNum)) logger = do
    let textConnStr = decodeUtf8 connStr
        LoggingT runConnPool = createSqlitePool textConnStr connNum
    runConnPool logger

runSqlite ::
     MonadUnliftIO m
  => Config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> ReaderT SqlBackend m a
  -> m a
runSqlite conf@(Config (DbConnStr connStr) _) logger query
  | connStr == ":memory:" = undefined
  | otherwise = do
    pool <- liftIO $ genConnectionPool conf logger
    runSqlPool query pool

init :: forall config backend.
     (TonaDbConfig config)
  => config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO Shared
init conf logger = TonaDb.init conf logger runSqlite
