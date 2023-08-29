{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Tonatona.Servant
  ( Tonatona.Servant.run
  , runWithHandlers
  , redirect
  , Config(..)
  , Host(..)
  , Port
  , Protocol(..)
  ) where

import RIO

import Data.Default (def)
import Data.Kind (Type)
import Network.HTTP.Types.Header
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (OutputFormat(..), logStdout, logStdoutDev, mkRequestLogger, outputFormat)
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSONWithHeaders)
import Servant

import TonaParser (Parser, (.||), argLong, envVar, optionalVal)
import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger

{-| Main function.
 -}
run ::
     forall (api :: Type) env.
     (HasServer api '[], HasConfig env Config, HasConfig env TonaLogger.Config)
  => ServerT api (RIO env)
  -> RIO env ()
run =
  runWithHandlers @api []

{-| Main function which allows you to pass error handlers.
 -}
runWithHandlers ::
     forall (api :: Type) env.
     (HasServer api '[], HasConfig env Config, HasConfig env TonaLogger.Config)
  => (forall a. [RIO.Handler (RIO env) a])
  -> ServerT api (RIO env)
  -> RIO env ()
runWithHandlers handlers servantServer = do
  env <- ask
  conf <- asks config
  loggingMiddleware <- reqLogMiddleware
  let app = runServant @api env handlers servantServer
  liftIO $ Warp.run (port conf) $ loggingMiddleware app

runServant ::
     forall (api :: Type) env. (HasServer api '[])
  => env
  -> (forall a. [RIO.Handler (RIO env) a])
  -> ServerT api (RIO env)
  -> Application
runServant env handlers servantServer =
  serve (Proxy @api) $ hoistServer (Proxy @api) (transformation . t) servantServer
  where
    t :: forall a. RIO env a -> RIO env a
    t = flip catches handlers
    transformation
      :: forall a. RIO env a -> Servant.Handler a
    transformation action = do
      let
        ioAction = Right <$> runRIO env action
      eitherRes <- liftIO $ ioAction `catch` \(e :: ServerError) -> pure $ Left e
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
  case (mode, verbose) of
    (TonaLogger.Development, TonaLogger.Verbose True) ->
      liftIO mkLogStdoutVerbose
    (TonaLogger.Development, TonaLogger.Verbose False) ->
      pure logStdoutDev
    (_, TonaLogger.Verbose True) ->
      pure logStdoutDev
    (_, TonaLogger.Verbose False) ->
      pure logStdout

mkLogStdoutVerbose :: IO Middleware
mkLogStdoutVerbose = do
  mkRequestLogger def
    { outputFormat = CustomOutputFormatWithDetailsAndHeaders formatAsJSONWithHeaders
    }

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
