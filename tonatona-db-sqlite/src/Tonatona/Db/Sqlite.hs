{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Tonatona.Db.Sqlite
  ( run
  , TonaDbM
  , Config(..)
  , DbConnStr(..)
  , DbConnNum(..)
  , HasConfig(..)
  , Shared
  , Tonatona.Db.Sqlite.init
  , HasShared(..)
  , runMigrate
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (Loc, LoggingT(..), LogLevel, LogSource, LogStr)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Pool (Pool)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Sqlite (createSqlitePool, wrapConnection)
import Database.Persist.Sql (Migration, SqlBackend, runMigration, runSqlPool)
import Database.Sqlite (open)
import Tonatona (TonaM, readerShared)
import Tonatona.Db (Config(..), DbConnStr(..), DbConnNum(..), HasConfig(..))

type TonaDbM conf shared
  = ReaderT SqlBackend (TonaM conf shared)

genConnectionPool ::
     Config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO (Pool SqlBackend)
genConnectionPool (Config (DbConnStr connStr) (DbConnNum connNum)) logger = do
    let textConnStr = decodeUtf8 connStr
        LoggingT runConnPool = createSqlitePool textConnStr connNum
    runConnPool logger

-- TODO: Add function for freeing the pool.
init :: forall config.
     HasConfig config
  => config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO Shared
init conf logger =
  case dbConnString (config conf) of
    ":memory:" -> do
      conn <- open ":memory:"
      backend <- wrapConnection conn logger
      pure $ Shared (SqliteConn backend)
    _ -> do
      pool <- liftIO $ genConnectionPool (config conf) logger
      pure $ Shared (SqliteConnPool pool)

class HasShared shared where
  shared :: shared -> Shared

data SqliteConn
  = SqliteConn SqlBackend
  | SqliteConnPool (Pool SqlBackend)

data Shared = Shared
  { sqliteConn :: SqliteConn
  }

runMigrate :: HasShared shared => Migration -> TonaM conf shared ()
runMigrate migration = run $ runMigration migration

-- | Main function.
run :: HasShared shared => TonaDbM conf shared a -> TonaM conf shared a
run query = do
  connType <- readerShared (sqliteConn . shared)
  case connType of
    SqliteConn sqlBackend -> runReaderT query sqlBackend
    SqliteConnPool pool -> runSqlPool query pool
