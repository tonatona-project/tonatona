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
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))
import Data.String
import Data.Text
import Network.HTTP.Types.Header
import Servant
import System.Envy (FromEnv(..), Var(..), (.!=), decodeEnv, env, envMaybe)
import Tonatona
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp

{-| Main function.
 -}
run :: forall conf shared api. (HasServer api '[], Plug conf shared, TonaServantConfig conf) => ServerT api (TonaM conf shared) -> IO ()
run servantServer = do
  mconf <- decodeEnv
  case mconf of
    Left err -> error $ "Fail to decode env: " <> err
    Right conf -> do
      shared <- Tonatona.init conf
      Warp.run (port (config conf)) $ runServant @conf @shared @api conf shared servantServer
      -- runReaderT ma (conf, shared)

redirect :: ByteString -> TonaM conf shared a
redirect redirectLocation =
  throwM $
    err302
      { errHeaders = [(hLocation, redirectLocation)]
      }

runServant :: forall conf shared api. HasServer api '[] => conf -> shared -> ServerT api (TonaM conf shared) -> Application
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
