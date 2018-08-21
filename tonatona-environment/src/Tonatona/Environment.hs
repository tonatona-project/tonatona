module Tonatona.Environment
  ( Config(..)
  , HasConfig(..)
  , Environment(..)
  ) where

import GHC.Generics (Generic)
import Text.Read (readMaybe)

import TonaParser (FromEnv(..), Var(..), (.||), argLong, argShort, envDef, envVar)

data Config = Config
  { environment :: Environment
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv =
    let env =
          envDef
            ( argLong "env" .||
              argShort 'e' .||
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
