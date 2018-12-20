{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Tonatona.Logger
  ( Config(..)
  , DeployMode(..)
  , Verbose(..)
  , defaultVerbosity
    -- * Standard logging functions
  , Tonatona.Logger.logDebug
  , Tonatona.Logger.logInfo
  , Tonatona.Logger.logWarn
  , Tonatona.Logger.logError
  , Tonatona.Logger.logOther
    -- * Advanced logging functions
    -- ** Sticky logging
  , Tonatona.Logger.logSticky
  , Tonatona.Logger.logStickyDone
    -- ** With source
  , Tonatona.Logger.logDebugS
  , Tonatona.Logger.logInfoS
  , Tonatona.Logger.logWarnS
  , Tonatona.Logger.logErrorS
  , Tonatona.Logger.logOtherS
    -- ** Generic log function
  , Tonatona.Logger.logGeneric
    -- * Data types
  , LogLevel (..)
  , LogSource
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
  , optionalEnum
  )


-- Standard logging functions


{- | Log a debug level message with no source.
-}
logDebug :: (HasConfig env Config) => Utf8Builder -> RIO env ()
logDebug = unwrap . RIO.logDebug

{- | Log an info level message with no source.
-}
logInfo :: (HasConfig env Config) => Utf8Builder -> RIO env ()
logInfo = unwrap . RIO.logInfo

{- | Log a warn level message with no source.
-}
logWarn :: (HasConfig env Config) => Utf8Builder -> RIO env ()
logWarn = unwrap . RIO.logWarn

{- | Log an error level message with no source.
-}
logError :: (HasConfig env Config) => Utf8Builder -> RIO env ()
logError = unwrap . RIO.logError

{- | Log a message with the specified textual level and no source.
-}
logOther :: (HasConfig env Config)
  => Text -- ^ level
  -> Utf8Builder -> RIO env ()
logOther level = unwrap . RIO.logOther level



-- With source


{- | Log a debug level message with the given source.
-}
logDebugS
  :: (HasConfig env Config)
  => LogSource
  -> Utf8Builder
  -> RIO env ()
logDebugS src = unwrap . RIO.logDebugS src

{- | Log an info level message with the given source.
-}
logInfoS
  :: (HasConfig env Config)
  => LogSource
  -> Utf8Builder
  -> RIO env ()
logInfoS src = unwrap . RIO.logInfoS src

{- | Log a warn level message with the given source.
-}
logWarnS
  :: (HasConfig env Config)
  => LogSource
  -> Utf8Builder
  -> RIO env ()
logWarnS src = unwrap . RIO.logWarnS src

{- | Log an error level message with the given source.
-}
logErrorS
  :: (HasConfig env Config)
  => LogSource
  -> Utf8Builder
  -> RIO env ()
logErrorS src = unwrap . RIO.logErrorS src

{- | Log a message with the specified textual level and the given source.
-}
logOtherS
  :: (HasConfig env Config)
  => Text -- ^ level
  -> LogSource
  -> Utf8Builder
  -> RIO env ()
logOtherS level src = unwrap . RIO.logOtherS level src

{- | Write a "sticky" line to the terminal. Any subsequent lines will
  overwrite this one, and that same line will be repeated below
  again. In other words, the line sticks at the bottom of the output
  forever. Running this function again will replace the sticky line
  with a new sticky line. When you want to get rid of the sticky
  line, run 'logStickyDone'.

  Note that not all 'LogFunc' implementations will support sticky
  messages as described. However, the 'withLogFunc' implementation
  provided by this module does.
-}
logSticky :: (HasConfig env Config) => Utf8Builder -> RIO env ()
logSticky = unwrap . RIO.logSticky

{- | This will print out the given message with a newline and disable
  any further stickiness of the line until a new call to 'logSticky'
  happens.
-}
logStickyDone :: (HasConfig env Config) => Utf8Builder -> RIO env ()
logStickyDone = unwrap . RIO.logStickyDone



-- Generic log function


{- | Generic, basic function for creating other logging functions.
-}
logGeneric ::
     (HasConfig env Config)
  => LogSource
  -> LogLevel
  -> Utf8Builder
  -> RIO env ()
logGeneric src level str = unwrap $ RIO.logGeneric src level str


unwrap :: RIO (InnerEnv env) () -> RIO env ()
unwrap action = do
  env <- ask
  runRIO (InnerEnv env) action


newtype InnerEnv env = InnerEnv { unInnerEnv :: env }


instance (HasConfig env Config) => HasLogFunc (InnerEnv env) where
  logFuncL = lens (logFunc . config . unInnerEnv) $
    error "Setter for logFuncL is not defined"


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


-- Verbose


newtype Verbose = Verbose { unVerbose :: Bool }
  deriving (Show, Read, Eq)

instance HasParser Verbose where
  parser = Verbose <$>
    optionalEnum
      "Make the operation more talkative"
      (argLong "verbose" .|| envVar "VERBOSE")
      False


-- DeployMode


data DeployMode
  = Development
  | Production
  | Staging
  | Test
  deriving (Eq, Generic, Show, Read, Bounded, Enum)

instance Var DeployMode where
  toVar = show
  fromVar = readMaybe

instance HasParser DeployMode where
  parser =
    optionalEnum
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
