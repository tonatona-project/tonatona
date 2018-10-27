{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Tonatona.Servant
  ( Tonatona.Servant.run
  , redirect
  , Config(..)
  , HasConfig(..)
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

import TonaParser (FromEnv(..), Var(..), (.||), argLong, envDef, envVar)
import Tonatona (TonaM)

reqLogMiddleware :: HasConfig conf => TonaM conf shared Middleware
reqLogMiddleware = do
  logger <- asks (reqLog . config . fst)
  case logger of
    ReqLogVerbose -> pure logStdoutDev
    ReqLogNormal -> pure logStdout
    ReqLogQuiet -> pure id

{-| Main function.
 -}
run ::
     forall (api :: *) conf shared.
     (HasServer api '[], HasConfig conf)
  => ServerT api (TonaM conf shared)
  -> TonaM conf shared ()
run servantServer = do
  (conf, shared) <- ask
  loggingMiddleware <- reqLogMiddleware
  let app = runServant @api conf shared servantServer
  liftIO $ Warp.run (port (config conf)) $ loggingMiddleware app

runServant ::
     forall (api :: *) conf shared. HasServer api '[]
  => conf
  -> shared
  -> ServerT api (TonaM conf shared)
  -> Application
runServant conf shared servantServer =
  serve (Proxy @api) $ hoistServer (Proxy @api) transformation servantServer
  where
    transformation
      :: forall a. TonaM conf shared a -> Servant.Handler a
    transformation action = do
      let
        -- ioAction :: IO (Either ServantErr a)
        ioAction = Right <$> runRIO (conf, shared) action
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
  fromVar _ = Nothing

data Config = Config
  { host :: Host
  , protocol :: Protocol
  , port :: Port
  , reqLog :: ReqLog
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv =
    let host =
          envDef
            ( argLong "host" .||
              envVar "HOST"
            )
            ("localhost" :: Host)
        protocol =
          envDef
            ( argLong "protocol" .||
              envVar "PROTOCOL"
            )
            ("http" :: Protocol)
        port =
          envDef
            ( argLong "port" .||
              envVar "PORT"
            )
            (8000 :: Port)
        reqlog =
          envDef
            ( argLong "reqlog" .||
              envVar "REQLOG"
            )
            ReqLogVerbose
    in Config <$> host <*> protocol <*> port <*> reqlog

class HasConfig config where
  config :: config -> Config
