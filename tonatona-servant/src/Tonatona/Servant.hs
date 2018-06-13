{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Tonatona.Servant
  ( Tonatona.Servant.run
  , redirect
  , Config(..)
  , TonaServantConfig(..)
  ) where

import Control.Exception (catch)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, reader, runReaderT)
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))
import Data.String
import Data.Text
import Network.HTTP.Types.Header
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant
import System.Envy (FromEnv(..), Var(..), (.!=), decodeEnv, env, envMaybe)
import Tonatona

reqLogMiddleware :: TonaServantConfig conf => TonaM conf shared Middleware
reqLogMiddleware = do
  logger <- reader (reqLog . config . fst)
  case logger of
    ReqLogVerbose -> pure logStdoutDev
    ReqLogNormal -> pure logStdout
    ReqLogQuiet -> pure id

{-| Main function.
 -}
run ::
     forall api conf shared.
     (HasServer api '[], Plug conf shared, TonaServantConfig conf)
  => ServerT api (TonaM conf shared)
  -> TonaM conf shared ()
run servantServer = do
  (conf, shared) <- ask
  loggingMiddleware <- reqLogMiddleware
  let app = runServant @api conf shared servantServer
  liftIO $ Warp.run (port (config conf)) $ loggingMiddleware app

runServant ::
     forall api conf shared. HasServer api '[]
  => conf
  -> shared
  -> ServerT api (TonaM conf shared)
  -> Application
runServant conf shared servantServer =
  serve (Proxy @api) $ hoistServer (Proxy @api) transformation servantServer
  where
    transformation
      :: forall a. TonaM conf shared a -> Handler a
    transformation action = do
      let ioAction = Right <$> runReaderT action (conf, shared)
      eitherRes <- liftIO $ ioAction `catch` \(e :: ServantErr) -> pure $ Left e
      case eitherRes of
        Right res -> pure res
        Left servantErr -> throwError servantErr

redirect :: ByteString -> TonaM conf shared a
redirect redirectLocation =
  throwM $
    err302
      { errHeaders = [(hLocation, redirectLocation)]
      }

-- Config

-- | This defines the host part of a URL.
--
-- For example, in the URL https://some.url.com:8090/, the host is
-- @some.url.com@.
newtype Host = Host
  { unHost :: Text
  } deriving (Eq, IsString, Read, Show, Var)

-- | This defines the protocol part of a URL.
--
-- For example, in the URL https://some.url.com:8090/, the protocol is
-- @https@.
newtype Protocol = Protocol
  { unProtocol :: Text
  } deriving (Eq, IsString, Read, Show, Var)

data ReqLog
  = ReqLogVerbose
  | ReqLogNormal
  | ReqLogQuiet
  deriving (Eq, Read, Show)

instance Var ReqLog where
  toVar :: ReqLog -> String
  toVar ReqLogVerbose = "verbose"
  toVar ReqLogNormal = "normal"
  toVar ReqLogQuiet = "quiet"

  fromVar :: String -> Maybe ReqLog
  fromVar "verbose" = Just ReqLogVerbose
  fromVar "normal" = Just ReqLogNormal
  fromVar "quiet" = Just ReqLogQuiet

data Config = Config
  { host :: Host
  , protocol :: Protocol
  , port :: Port
  , reqLog :: ReqLog
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { host = "localhost"
    , protocol = "http"
    , port = 8000
    , reqLog = ReqLogVerbose
    }

instance FromEnv Config where
  fromEnv = Config
    <$> envMaybe "TONA_SERVANT_HOST" .!= "localhost"
    <*> envMaybe "TONA_SERVANT_PROTOCOL" .!= "http"
    <*> envMaybe "TONA_SERVANT_PORT" .!= 8000
    <*> envMaybe "TONA_SERVANT_REQLOG" .!= ReqLogVerbose

class TonaServantConfig config where
  config :: config -> Config
