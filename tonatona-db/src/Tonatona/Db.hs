module Tonatona.Db
  ( run
  , Config(..)
  , Shared(..)
  , Tonatona.Db.init
  , TonaDbM
  , TonaDbConfig(..)
  , TonaDbShared(..)
  , migrate
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Semigroup ((<>))
import System.Envy (FromEnv(..), (.!=), env, envMaybe)
import Tonatona (TonaM)
import Tonatona.Environment (TonaEnvConfig)
import qualified Tonatona.Environment as TonaEnv

{-| Main type
 - TODO make this an opaque type, and appropreate Monad instead of `IO`
 -}
type TonaDbM conf shared a
   = (TonaDbConfig conf, TonaEnvConfig conf, TonaDbShared shared) =>
       ReaderT (conf, shared) IO a

{-| Main function.
 -}
run :: (TonaDbConfig conf, TonaEnvConfig conf, TonaDbShared shared) => TonaDbM conf shared a -> TonaM conf shared a
run = id

migrate :: (TonaDbConfig conf, TonaEnvConfig conf) => TonaDbM conf shared ()
migrate = do
  (conf, shared') <- ask
  let env' = TonaEnv.environment $ TonaEnv.config conf
  liftIO $
    putStrLn $
    "This function can use shared connection pool: " <>
    (dbPool . shared $ shared')
  liftIO $
    putStrLn $
    "This function can use ENV environment variable to decide behaviour: " <> show env'
  liftIO $ putStrLn $ "Migrating: " <> (dbString . config $ conf)


-- Shared

-- Dummy type for demonstration
type ConnectionPool = String

class TonaDbShared shared where
  shared :: shared -> Shared

data Shared = Shared
  { dbPool :: ConnectionPool
  }
  deriving (Show)

init :: (TonaDbConfig config) => config -> IO Shared
init conf = Shared
  <$> genConnectionPool (config conf)

genConnectionPool :: Config -> IO ConnectionPool
genConnectionPool Config {dbString} = do
  putStrLn "Generationg dummy Connection Pool..."
  print dbString
  pure "Dummy Connection Pool"


-- Config

data Config = Config
  { dbString :: String
  }
  deriving (Show)


instance FromEnv Config where
  fromEnv = Config
    <$> envMaybe "TONA_DB_DB_STRING" .!= "postgresql://myuser:mypass@localhost:5432/mydb"

class TonaDbConfig config where
  config :: config -> Config
