module Tonatona.Sample
  ( app
  ) where

import Tonatona (Plug(..))
import qualified Tonatona as Tona
import Tonatona.Db (TonaDbConfig(..), TonaDbShared(..))
import qualified Tonatona.Db as TonaDb
import System.Envy (FromEnv(..))

app :: IO ()
app = Tona.run @Config @Shared $ do
  TonaDb.run $
    TonaDb.migrate
  -- dbPool is shared
  TonaDb.run $
    TonaDb.migrate


-- Config

data Config = Config
  { tonaDb :: TonaDb.Config
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv = Config
    <$> fromEnv

instance TonaDbConfig Config where
  config = tonaDb


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
