{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tonatona.Db
  -- ( run
  -- , Config(..)
  -- , Shared(..)
  -- , Tonatona.Db.init
  -- , TonaDbM
  -- , TonaDbConfig(..)
  -- , TonaDbShared(..)
  -- , runMigrate
  -- ) where
  where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT, reader)
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))
import Data.String (IsString)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Migration, SqlBackend, runMigration, runSqlPool)
import System.Envy (FromEnv(..), Var, (.!=), env, envMaybe)
import Tonatona (TonaM)
import Tonatona.Environment (TonaEnvConfig)
import qualified Tonatona.Environment as TonaEnv
import UnliftIO

{-| Main type
 - TODO make this an opaque type, and appropreate Monad instead of `IO`
 -}
type TonaDbM conf shared
  = ReaderT SqlBackend (TonaM conf shared)

{-| Main function.
 -}
run :: (TonaDbShared shared) => TonaDbM conf shared a -> TonaM conf shared a
run query = do
  f <- reader (runDb . shared . snd)
  f query

runMigrate :: (TonaDbShared shared) => Migration -> TonaM conf shared ()
runMigrate migration = run $ runMigration migration

-- Config

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
    Config
      <$> envMaybe "TONA_DB_CONN_STRING" .!= "postgresql://myuser:mypass@localhost:5432/mydb"
      <*> envMaybe "TONA_DB_CONN_NUM" .!= 10

class TonaDbConfig config where
  config :: config -> Config

-- Shared

class TonaDbShared shared where
  shared :: shared -> Shared

data Shared = Shared
  { runDb :: forall m a. MonadUnliftIO m => ReaderT SqlBackend m a -> m a
  }

init ::
     (TonaDbConfig config)
  => config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO Shared
init conf logger = pure $ Shared (runPostgres (config conf) logger)

genConnectionPool ::
     Config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> IO ConnectionPool
genConnectionPool (Config (DbConnStr connStr) (DbConnNum connNum)) logger = do
  let LoggingT runConnPool = createPostgresqlPool connStr connNum
  runConnPool logger

runPostgres ::
     (MonadIO m, MonadUnliftIO m)
  => Config
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> ReaderT SqlBackend m a
  -> m a
runPostgres conf logger query = do
  pool <- liftIO $ genConnectionPool conf logger
  runSqlPool query pool
