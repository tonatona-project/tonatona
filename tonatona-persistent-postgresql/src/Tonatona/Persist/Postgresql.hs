module Tonatona.Persist.Postgresql
  ( run
  , TonaDbM
  , Config(..)
  , DbConnStr(..)
  , DbConnNum(..)
  , runMigrate
  ) where

import RIO

import Control.Monad.Logger as Logger (Loc, LoggingT(..), LogLevel, LogSource, LogStr, runStdoutLoggingT)
import Data.Pool (Pool)
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (Migration, SqlBackend, runMigration, runSqlPool)

import Tonatona (HasConfig(..), HasParser(..))
import TonaParser ((.||), argLong, envVar, liftWith, optionalVal)

type TonaDbM env
  = ReaderT SqlBackend (RIO env)

runMigrate :: (HasConfig env Config) => Migration -> RIO env ()
runMigrate migration = run $ runMigration migration

-- | Main function.
run :: (HasConfig env Config) => TonaDbM env a -> RIO env a
run query = do
  pool <- asks (connPool . config)
  runSqlPool query pool

------------
-- Config --
------------

newtype DbConnStr = DbConnStr
  { unDbConnStr :: ByteString
  } deriving (Eq, IsString, Read, Show)

instance HasParser DbConnStr where
  parser = DbConnStr <$>
    optionalVal
      "Formatted string to connect postgreSQL"
      (argLong "db-conn-string" .|| envVar "DB_CONN_STRING")
      "postgresql://myuser:mypass@localhost:5432/mydb"

newtype DbConnNum = DbConnNum { unDbConnNum :: Int }
  deriving (Eq, Num, Read, Show)

instance HasParser DbConnNum where
  parser = DbConnNum <$>
    optionalVal
      "Number of connections which connection pool uses"
      ( argLong "db-conn-num" .|| envVar "DB_CONN_NUM")
      10

data Config = Config
  { connString :: DbConnStr
  , connNum :: DbConnNum
  , connPool :: Pool SqlBackend
  }
  deriving (Show)

instance HasParser Config where
  parser = do
    connStr <- parser
    connNum <- parser
    liftWith $ \action -> do
      runLoggingT
        (withPostgresqlPool
           (unDbConnStr connStr)
           (unDbConnNum connNum)
           (lift . action . Config connStr connNum))
        stdoutLogger

stdoutLogger :: Loc -> Logger.LogSource -> Logger.LogLevel -> LogStr -> IO ()
stdoutLogger loc source level msg = do
  func <- runStdoutLoggingT $ LoggingT pure
  func loc source level msg
