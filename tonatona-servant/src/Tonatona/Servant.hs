{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Tonatona.Servant
  ( Tonatona.Servant.run
  , redirect
  , Config(..)
  , Host(..)
  , Port
  , Protocol(..)
  ) where

import RIO

import Network.HTTP.Types.Header
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant

import TonaParser (Parser, (.||), argLong, envVar, optionalVal)
import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger

{-| Main function.
 -}
run ::
     forall (api :: *) env.
     (HasServer api '[], HasConfig env Config, HasConfig env TonaLogger.Config)
  => ServerT api (RIO env)
  -> RIO env ()
run servantServer = do
  env <- ask
  conf <- asks config
  loggingMiddleware <- reqLogMiddleware
  let app = runServant @api env servantServer
  liftIO $ Warp.run (port conf) $ loggingMiddleware app

runServant ::
     forall (api :: *) env. (HasServer api '[])
  => env
  -> ServerT api (RIO env)
  -> Application
runServant env servantServer =
  serve (Proxy @api) $ hoistServer (Proxy @api) transformation servantServer
  where
    transformation
      :: forall a. RIO env a -> Servant.Handler a
    transformation action = do
      let
        ioAction = Right <$> runRIO env action
      eitherRes <- liftIO $ ioAction `catch` \(e :: ServantErr) -> pure $ Left e
      case eitherRes of
        Right res -> pure res
        Left servantErr -> throwError servantErr

redirect :: ByteString -> RIO env a
redirect redirectLocation =
  throwM $
    err302
      { errHeaders = [(hLocation, redirectLocation)]
      }

reqLogMiddleware :: (HasConfig env TonaLogger.Config) => RIO env Middleware
reqLogMiddleware = do
  TonaLogger.Config {mode, verbose} <- asks config
  pure $
    if TonaLogger.defaultVerbosity mode verbose
      then logStdoutDev
      else logStdout

-- Config

-- | This defines the host part of a URL.
--
-- For example, in the URL https://some.url.com:8090/, the host is
-- @some.url.com@.
newtype Host = Host
  { unHost :: Text
  } deriving (Eq, IsString, Read, Show)

-- | This defines the protocol part of a URL.
--
-- For example, in the URL https://some.url.com:8090/, the protocol is
-- @https@.
newtype Protocol = Protocol
  { unProtocol :: Text
  } deriving (Eq, IsString, Read, Show)

data Config = Config
  { host :: Host
  , protocol :: Protocol
  , port :: Port
  }
  deriving (Show)

instance HasParser Host where
  parser = Host <$>
    optionalVal
      "Host name to serve"
      (argLong "host" .|| envVar "HOST")
      "localhost"

instance HasParser Protocol where
  parser = Protocol <$>
    optionalVal
      "Protocol to serve"
      (argLong "protocol" .|| envVar "PROTOCOL")
      "http"

portParser :: Parser Port
portParser =
  optionalVal
    "Port to serve"
    (argLong "port" .|| envVar "PORT")
    8000

instance HasParser Config where
  parser = Config
    <$> parser
    <*> parser
    <*> portParser
