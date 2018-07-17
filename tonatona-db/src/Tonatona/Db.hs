{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tonatona.Db
  ( DbConnStr(..)
  , DbConnNum(..)
  , Config(..)
  , HasConfig(..)
  ) where

import Control.Monad.Reader (ReaderT, reader)
import Data.ByteString (ByteString)
import Data.String (IsString)
import System.Envy (FromEnv(..), Var, (.!=), envMaybe)
import Tonatona (TonaM)

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
      <$> envMaybe "TONA_DB_CONN_STRING" .!= "postgresql://myuser:mypass@localhost:5432/mydb"
      <*> envMaybe "TONA_DB_CONN_NUM" .!= 10

class HasConfig config where
  config :: config -> Config
