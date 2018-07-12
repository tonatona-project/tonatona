module Tonatona.Environment
  ( Config(..)
  , HasConfig(..)
  , Environment(..)
  ) where

import GHC.Generics (Generic)
import Text.Read (readMaybe)
import System.Envy (FromEnv(..), (.!=), envMaybe)
import qualified System.Envy as Envy

data Config = Config
  { environment :: Environment
  }
  deriving (Show)


instance FromEnv Config where
  fromEnv = Config
    <$> envMaybe "ENV" .!= Development

class HasConfig config where
  config :: config -> Config

data Environment
  = Development
  | Production
  | Staging
  | Test
  deriving (Eq, Generic, Show, Read)

instance Envy.Var Environment where
  toVar = show
  fromVar = readMaybe
