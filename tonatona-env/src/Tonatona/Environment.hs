module Tonatona.Environment
  ( Config(..)
  , Environment(..)
  ) where

import GHC.Generics (Generic)
import Text.Read (readMaybe)
import System.Envy (FromEnv(..), env)
import qualified System.Envy as Envy

data Config = Config
  { environment :: Environment
  }
  deriving (Show)


instance FromEnv Config where
  fromEnv = Config
    <$> env "ENV"

data Environment
  = Development
  | Production
  | Staging
  | Test
  deriving (Eq, Generic, Show, Read)

instance Envy.Var Environment where
  toVar = show
  fromVar = readMaybe
