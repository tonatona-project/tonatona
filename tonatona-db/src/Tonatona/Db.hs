{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tonatona.Db
  ( run
  , Config(..)
  , Shared(..)
  , Tonatona.Db.init
  , TonaDbM
  , TonaDbConfig(..)
  , TonaDbShared(..)
  , runMigrate
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
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

{-| Main type
 - TODO make this an opaque type, and appropreate Monad instead of `IO`
 -}
type TonaDbM conf shared
  = ReaderT SqlBackend (TonaM conf shared)

{-| Main function.
 -}
run :: (TonaDbShared shared) => TonaDbM conf shared a -> TonaM conf shared a
run query = do
  pool <- reader (dbPool . shared . snd)
  runSqlPool query pool

runMigrate :: (TonaDbShared shared) => Migration -> TonaM conf shared ()
runMigrate migration = run $ runMigration migration

-- Shared

-- Dummy type for demonstration
class TonaDbShared shared where
  shared :: shared -> Shared

data Shared = Shared
  { dbPool :: ConnectionPool
  }
  deriving (Show)

init :: (TonaDbConfig config) => config -> IO Shared
init conf = Shared <$> genConnectionPool (config conf)

genConnectionPool :: Config -> IO ConnectionPool
genConnectionPool (Config (DbConnStr connStr) (DbConnNum connNum)) =
  -- TODO: Replace this logging call to the one from Tonatona.Logging
  runStdoutLoggingT $ createPostgresqlPool connStr connNum

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
      <$> envMaybe "TONA_DB_DB_CONN_STRING" .!= "postgresql://myuser:mypass@localhost:5432/mydb"
      <*> envMaybe "TONA_DB_DB_CONN_NUM" .!= 10

class TonaDbConfig config where
  config :: config -> Config
