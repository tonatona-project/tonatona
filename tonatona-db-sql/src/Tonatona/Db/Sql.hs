{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tonatona.Db.Sql
  ( TonaDb.run
  , TonaDbM
  , Config(..)
  , DbConnStr(..)
  , DbConnNum(..)
  , TonaDb.TonaDbConfig(..)
  , SharedSql
  , TonaDb.Shared(Shared)
  , TonaDbSqlShared(..)
  , runMigrate
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT, reader)
import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Semigroup ((<>))
import Data.String (IsString)
import Database.Persist.Sql (ConnectionPool, Migration, SqlBackend, runMigration, runSqlPool)
import System.Envy (FromEnv(..), Var, (.!=), env, envMaybe)
import Tonatona (TonaM)
import Tonatona.Db (Config(..), DbConnStr(..), DbConnNum(..), TonaDbShared)
import qualified Tonatona.Db as TonaDb
import Tonatona.Environment (TonaEnvConfig)
import qualified Tonatona.Environment as TonaEnv
import UnliftIO

type TonaDbM conf shared
  = ReaderT SqlBackend (TonaM conf shared)

type SharedSql = TonaDb.Shared SqlBackend

runMigrate :: (TonaDbSqlShared shared) => Migration -> TonaM conf shared ()
runMigrate migration = TonaDb.run $ runMigration migration

class TonaDbSqlShared shared where
  shared :: shared -> SharedSql

instance {-# OVERLAPPABLE #-} TonaDbSqlShared shared => TonaDbShared SqlBackend shared where
  shared = Tonatona.Db.Sql.shared
