module Tonatona.Db
  ( run
  , Config(..)
  , TonaDbM
  , TonaDbConfig(..)
  , migrate
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Semigroup ((<>))
import System.Envy (FromEnv(..), env)
import Tonatona (TonaConfig, TonaM)

{-| Main type
 - TODO make this an opaque type, and appropreate Monad instead of `IO`
 -}
type TonaDbM conf a
   = (TonaDbConfig conf) =>
       ReaderT conf IO a

{-| Main function.
 -}
run :: TonaDbConfig conf => TonaDbM conf a -> TonaM conf a
run = id

migrate :: TonaDbConfig conf => TonaDbM conf ()
migrate = do
  conf <- ask
  liftIO $ putStrLn $ "Migrating: " <> (dbString . getConfig $ conf)

data Config = Config
  { dbString :: String
  }
  deriving (Show)


instance FromEnv Config where
  fromEnv = Config
    <$> env "TONA_DB_DB_STRING"

class (TonaConfig config) => TonaDbConfig config where
  getConfig :: config -> Config
