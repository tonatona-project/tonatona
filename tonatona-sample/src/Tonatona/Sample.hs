module Tonatona.Sample
  ( app
  ) where

import Data.Semigroup ((<>))
import Tonatona (Plug(..))
import qualified Tonatona as Tona
import qualified Tonatona.IO as TonaIO
import Tonatona.Db (TonaDbConfig(..), TonaDbShared(..))
import qualified Tonatona.Db as TonaDb
import Tonatona.Environment (TonaEnvConfig(..))
import qualified Tonatona.Environment as TonaEnv
import System.Envy (FromEnv(..))

app :: IO ()
app = Tona.run @Config @Shared $ do
  TonaDb.run $
    TonaDb.migrate
  TonaIO.run $ \_conf shared' ->
    putStrLn $ "dbPool (" <> TonaDb.dbPool (TonaDb.shared shared') <> ") is shared"
  TonaDb.run $
    TonaDb.migrate


-- Config

data Config = Config
  { tonaDb :: TonaDb.Config
  , tonaEnv :: TonaEnv.Config
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv = Config
    <$> fromEnv
    <*> fromEnv

instance TonaDbConfig Config where
  config = tonaDb

instance TonaEnvConfig Config where
  config = tonaEnv


-- Shared


data Shared = Shared
  { tonaDb :: TonaDb.Shared
  }
  deriving (Show)

instance Plug Config Shared where
  init conf = Shared
    <$> TonaDb.init conf

instance TonaDbShared Shared where
  shared = tonaDb
