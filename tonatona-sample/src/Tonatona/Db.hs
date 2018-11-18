module Tonatona.Db
  ( run
  , TonaDbM
  , Config(..)
  , DbToUse(..)
  , runMigrate
  ) where

import RIO

import Database.Persist.Sql (Migration, SqlBackend)

import Tonatona (HasConfig(..), HasParser(..))
import TonaParser (Parser(..), Var(..), (.||), argLong, envVar, optionalVal)
import qualified Tonatona.Persist.Postgresql as TonaDbPostgres
import qualified Tonatona.Persist.Sqlite as TonaDbSqlite

type TonaDbM env
  = ReaderT SqlBackend (RIO env)

runMigrate ::
     ( HasConfig env TonaDbSqlite.Config
     , HasConfig env TonaDbPostgres.Config
     , HasConfig env DbToUse
     )
  => Migration
  -> RIO env ()
runMigrate migration = do
  dbToUse <- asks config
  case dbToUse of
    Sqlite -> TonaDbSqlite.runMigrate migration
    PostgreSQL -> TonaDbPostgres.runMigrate migration

-- | Main function.
run ::
     ( HasConfig env TonaDbSqlite.Config
     , HasConfig env TonaDbPostgres.Config
     , HasConfig env DbToUse
     )
  => TonaDbM env a
  -> RIO env a
run query = do
  dbToUse <- asks config
  case dbToUse of
    Sqlite -> TonaDbSqlite.run query
    PostgreSQL -> TonaDbPostgres.run query


------------
-- Config --
------------

data Config = Config
  { dbToUse :: DbToUse
  , dbConfig :: DbConfig
  }

instance HasConfig Config DbToUse where
  config = dbToUse

instance HasConfig Config DbConfig where
  config = dbConfig

instance HasConfig Config TonaDbPostgres.Config where
  config c =
    case dbConfig c of
      DbPostgres a -> a
      _ -> error "This is not an postgres config type."

instance HasConfig Config TonaDbSqlite.Config where
  config c =
    case dbConfig c of
      DbSqlite a -> a
      _ -> error "This is not an sqlite config type."

instance HasParser a Config where
  parser = do
    dbToUse <- parser
    Config
      <$> parser
      <*> dbParser dbToUse

data DbConfig
  = DbPostgres TonaDbPostgres.Config
  | DbSqlite TonaDbSqlite.Config

data DbToUse = PostgreSQL | Sqlite deriving Show

instance HasParser a DbToUse where
  parser =
    optionalVal
      "Database type to use (postgresql|sqlite)"
      (argLong "db-to-use" .|| envVar "DB_TO_USE")
      PostgreSQL

instance Var DbToUse where
  toVar PostgreSQL = "postgresql"
  toVar Sqlite = "sqlite"

  fromVar "postgresql" = Just PostgreSQL
  fromVar "sqlite" = Just Sqlite
  fromVar _ = Nothing


dbParser :: DbToUse -> Parser a DbConfig
dbParser PostgreSQL = DbPostgres <$> parser
dbParser Sqlite = DbSqlite <$> parser
