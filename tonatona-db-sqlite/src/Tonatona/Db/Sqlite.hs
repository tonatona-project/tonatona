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
  , TonaDb.HasConfig(..)
  , Shared
  , Tonatona.Db.Sqlite.init
  , TonaDb.HasShared(..)
  , TonaDb.runMigrate
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Reader (runReaderT)
import Data.Pool (Pool)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Sqlite (createSqlitePool, wrapConnection)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Database.Sqlite (open)
import Tonatona.Db.Sql (Config(..), DbConnStr(..), DbConnNum(..), HasConfig(config), Shared, mkShared)
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
     HasConfig config
  => config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO Shared
init conf logger =
  case dbConnString (config conf) of
    ":memory:" -> do
      conn <- open ":memory:"
      backend <- wrapConnection conn logger
      pure $ mkShared $ \query -> runReaderT query backend
    _ -> do
      pool <- liftIO $ genConnectionPool (config conf) logger
      pure $ mkShared $ \query -> runSqlPool query pool
