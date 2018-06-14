{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import Control.Monad.Reader (ReaderT)
import Database.Persist.Sql (Migration, SqlBackend, runMigration)
import Tonatona (TonaM)
import Tonatona.Db (Config(..), DbConnStr(..), DbConnNum(..), TonaDbShared)
import qualified Tonatona.Db as TonaDb

type TonaDbM conf shared
  = ReaderT SqlBackend (TonaM conf shared)

type SharedSql = TonaDb.Shared SqlBackend

runMigrate :: (TonaDbSqlShared shared) => Migration -> TonaM conf shared ()
runMigrate migration = TonaDb.run $ runMigration migration

class TonaDbSqlShared shared where
  shared :: shared -> SharedSql

instance {-# OVERLAPPABLE #-} TonaDbSqlShared shared => TonaDbShared SqlBackend shared where
  shared = Tonatona.Db.Sql.shared
