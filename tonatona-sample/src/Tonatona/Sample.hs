module Tonatona.Sample
  ( app
  ) where

import Tonatona (TonaConfig(..))
import qualified Tonatona as Tona
import Tonatona.Db (TonaDbConfig(..))
import qualified Tonatona.Db as TonaDb
import System.Envy (FromEnv(..))

app :: IO ()
app = Tona.run @Config $ do
  TonaDb.run $
    TonaDb.migrate

-- Config

data Config = Config
  { tona :: Tona.Config
  , tonaDb :: TonaDb.Config
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv = Config
    <$> fromEnv
    <*> fromEnv

instance TonaConfig Config where
  getConfig = tona

instance TonaDbConfig Config where
  getConfig = tonaDb
