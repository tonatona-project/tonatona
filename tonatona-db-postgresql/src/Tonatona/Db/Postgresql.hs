{-# LANGUAGE DerivingStrategies #-}

module Tonatona.Db.Postgresql
  ( run
  , TonaDbM
  , Config(..)
  , DbConnStr(..)
  , DbConnNum(..)
  , HasConfig(..)
  , Shared
  , Tonatona.Db.Postgresql.init
  , HasShared(..)
  , runMigrate
  ) where

import RIO

import Control.Monad.Logger as Logger (Loc, LoggingT(..), LogLevel, LogSource, LogStr)
import Data.Pool (Pool)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (Migration, SqlBackend, runMigration, runSqlPool)

import TonaParser (FromEnv(..), Var(..), (.||), argLong, envDef, envVar)
import Tonatona (TonaM, asksShared)

type TonaDbM conf shared
  = ReaderT SqlBackend (TonaM conf shared)

genConnectionPool ::
     Config
  -> (Loc -> Logger.LogSource -> Logger.LogLevel -> LogStr -> IO ())
  -> IO (Pool SqlBackend)
genConnectionPool (Config (DbConnStr connStr) (DbConnNum connNum)) logger = do
  let LoggingT runConnPool = createPostgresqlPool connStr connNum
  runConnPool logger

-- TODO: Add function for freeing the pool.
init :: forall config.
     HasConfig config
  => config
  -> (Loc -> Logger.LogSource -> Logger.LogLevel -> LogStr -> IO ())
  -> IO Shared
init conf logger = do
  pool <- liftIO $ genConnectionPool (config conf) logger
  pure $ Shared pool

runMigrate :: HasShared shared => Migration -> TonaM conf shared ()
runMigrate migration = run $ runMigration migration

-- | Main function.
run :: HasShared shared => TonaDbM conf shared a -> TonaM conf shared a
run query = do
  connPool <- asksShared (connPool . shared)
  runSqlPool query connPool

------------
-- Config --
------------

newtype DbConnStr = DbConnStr
  { unDbConnStr :: ByteString
  } deriving newtype (Eq, IsString, Read, Show, Var)

newtype DbConnNum = DbConnNum { unDbConnNum :: Int }
  deriving newtype (Eq, Num, Read, Show, Var)

data Config = Config
  { dbConnString :: DbConnStr
  , dbConnNum :: DbConnNum
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv =
    let connStr =
          envDef
            ( argLong "db-conn-string" .||
              envVar "DB_CONN_STRING"
            )
            "postgresql://myuser:mypass@localhost:5432/mydb"
        connNum =
          envDef
            ( argLong "db-conn-num" .||
              envVar "DB_CONN_NUM"
            )
            10
    in Config <$> connStr <*> connNum

class HasConfig config where
  config :: config -> Config

------------
-- Shared --
------------

data Shared = Shared
  { connPool :: Pool SqlBackend
  }

class HasShared shared where
  shared :: shared -> Shared
