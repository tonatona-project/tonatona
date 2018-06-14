{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Tonatona.Db.Sqlite
  ( TonaDb.run
  , TonaDb.TonaDbM
  , Config(..)
  , DbConnStr(..)
  , DbConnNum(..)
  , TonaDb.TonaDbConfig(..)
  , Shared(..)
  , SharedSql
  , Tonatona.Db.Sqlite.init
  , TonaDb.TonaDbSqlShared(..)
  , TonaDb.runMigrate
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader (runReaderT)
import Data.Pool (Pool)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Sqlite (createSqlitePool, wrapConnection)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Database.Sqlite (open)
import Tonatona.Db.Sql (Config(..), DbConnStr(..), DbConnNum(..), Shared(..), SharedSql, TonaDbConfig(..))
import qualified Tonatona.Db.Sql as TonaDb

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
     (TonaDbConfig config)
  => config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO SharedSql
init conf logger =
  case dbConnString (config conf) of
    ":memory:" -> do
      conn <- open ":memory:"
      backend <- wrapConnection conn logger
      pure $ Shared $ \query -> runReaderT query backend
    _ -> do
      pool <- liftIO $ genConnectionPool (config conf) logger
      pure $ Shared $ \query -> runSqlPool query pool
