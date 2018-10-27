module Tonatona.Environment
  ( Config(..)
  , HasConfig(..)
  , Environment(..)
  ) where

import RIO

import GHC.Generics (Generic)
import TonaParser (FromEnv(..), Var(..), (.||), argLong, envDef, envVar)

data Config = Config
  { environment :: Environment
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv =
    let env =
          envDef
            ( argLong "env" .||
              envVar "ENV"
            )
            Development
    in Config <$> env

class HasConfig config where
  config :: config -> Config

data Environment
  = Development
  | Production
  | Staging
  | Test
  deriving (Eq, Generic, Show, Read)

instance Var Environment where
  toVar = show
  fromVar = readMaybe
