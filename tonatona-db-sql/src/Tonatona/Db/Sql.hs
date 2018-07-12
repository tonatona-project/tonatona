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
  , TonaDb.HasConfig(..)
  , Shared
  , mkShared
  , HasShared(..)
  , runMigrate
  ) where

import Control.Monad.Reader (ReaderT)
import Database.Persist.Sql (Migration, SqlBackend, runMigration)
import Tonatona (TonaM)
import Tonatona.Db (Config(..), DbConnStr(..), DbConnNum(..), mkShared)
import qualified Tonatona.Db as TonaDb

type TonaDbM conf shared
  = ReaderT SqlBackend (TonaM conf shared)

type Shared = TonaDb.Shared SqlBackend

runMigrate :: HasShared shared => Migration -> TonaM conf shared ()
runMigrate migration = TonaDb.run $ runMigration migration

class HasShared shared where
  shared :: shared -> Shared

instance {-# OVERLAPPABLE #-} HasShared shared => TonaDb.HasShared SqlBackend shared where
  shared = Tonatona.Db.Sql.shared
