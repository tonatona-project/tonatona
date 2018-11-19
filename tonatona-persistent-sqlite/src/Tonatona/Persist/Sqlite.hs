module Tonatona.Persist.Sqlite
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
import Database.Persist.Sqlite (withSqlitePool, wrapConnection)
import Database.Persist.Sql (Migration, SqlBackend, runMigration, runSqlPool)
import Database.Sqlite (open)

import Tonatona (HasConfig(..), HasParser(..))
import TonaParser ((.||), argLong, envVar, liftWith, optionalVal)

type TonaDbM env
  = ReaderT SqlBackend (RIO env)

runMigrate :: (HasConfig env Config) => Migration -> RIO env ()
runMigrate migration = run $ runMigration migration

-- | Main function.
run :: (HasConfig env Config) => TonaDbM env a -> RIO env a
run query = do
  connType <- asks (sqliteConn . config)
  case connType of
    SqliteConn sqlBackend -> runReaderT query sqlBackend
    SqliteConnPool pool -> runSqlPool query pool

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
      ":memory:"

newtype DbConnNum = DbConnNum { unDbConnNum :: Int }
  deriving (Eq, Num, Read, Show)

instance HasParser DbConnNum where
  parser = DbConnNum <$>
    optionalVal
      "Number of connections which connection pool uses"
      ( argLong "db-conn-num" .|| envVar "DB_CONN_NUM")
      10

data Config = Config
  { dbConnString :: DbConnStr
  , dbConnNum :: DbConnNum
  , sqliteConn :: SqliteConn
  }

instance HasParser Config where
  parser = do
    connStr <- parser
    connNum <- parser
    let textConnStr = decodeUtf8Lenient $ unDbConnStr connStr
    liftWith $ \action -> do
      case connStr of
        ":memory:" -> do
          conn <- open ":memory:"
          backend <- wrapConnection conn stdoutLogger
          action $
            Config connStr connNum (SqliteConn backend)
        _ ->
          runLoggingT
            (withSqlitePool
               textConnStr
               (unDbConnNum connNum)
               (lift . action . Config connStr connNum . SqliteConnPool))
            stdoutLogger

data SqliteConn
  = SqliteConn SqlBackend
  | SqliteConnPool (Pool SqlBackend)

stdoutLogger :: Loc -> Logger.LogSource -> Logger.LogLevel -> LogStr -> IO ()
stdoutLogger loc source level msg = do
  func <- runStdoutLoggingT $ LoggingT pure
  func loc source level msg
