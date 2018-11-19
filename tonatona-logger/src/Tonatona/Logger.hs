{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Tonatona.Logger
  ( Config(..)
  , DeployMode(..)
  , Verbose(..)
  , defaultVerbosity
  ) where

import RIO

import GHC.Generics (Generic)
import Tonatona (HasConfig(..), HasParser(..))
import TonaParser
  ( Var(..)
  , (.||)
  , argLong
  , envVar
  , liftWith
  , optionalVal
  )


-- Config


data Config = Config
  { mode :: DeployMode
  , verbose :: Verbose
  , logOptions :: LogOptions
  , logFunc :: LogFunc
  }

instance HasParser Config where
  parser = do
    mode <- parser
    verbose <- parser
    liftWith $ \action -> do
      options <- defaultLogOptions mode verbose
      withLogFunc options $ \lf ->
        action $ Config mode verbose options lf

instance (HasConfig env Config) => HasLogFunc env where
  logFuncL = lens (logFunc . config) $
    error "Setter for logFuncL is not defined"


-- Verbose


newtype Verbose = Verbose { unVerbose :: Bool }
  deriving (Show, Read, Eq)

instance HasParser Verbose where
  parser = Verbose <$>
    optionalVal
      "Make the operation more talkative"
      (argLong "verbose" .|| envVar "VERBOSE")
      False


-- DeployMode


data DeployMode
  = Development
  | Production
  | Staging
  | Test
  deriving (Eq, Generic, Show, Read)

instance Var DeployMode where
  toVar = show
  fromVar = readMaybe

instance HasParser DeployMode where
  parser =
    optionalVal
      "Application deployment mode to run"
      (argLong "env" .|| envVar "ENV")
      Development


-- Logger options


{-| Default way to create 'LogOptions'.
 -}
defaultLogOptions :: (MonadIO m) => DeployMode -> Verbose -> m LogOptions
defaultLogOptions env verbose = do
  logOptionsHandle stderr $ defaultVerbosity env verbose


{-| Default setting for verbosity.
 -}
defaultVerbosity :: DeployMode -> Verbose -> Bool
defaultVerbosity env (Verbose v) =
  case (v, env) of
    (False, Development) -> True
    _ -> v
