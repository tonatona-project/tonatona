{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import ClassyPrelude

import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant

$(share
  [ mkPersist sqlSettings {mpsGenerateLenses = False}
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
  BlogPost
    text Text

    deriving Eq
    deriving Show
    |]
 )

data DbEnv = DbEnv
  { dbEnvPool :: !ConnectionPool
  , dbEnvConf :: !DbConf
  } deriving show

data DbConf = DbConf
  { dbConfConnStr :: !DbConnStr
  , dbConfConnNum :: {-# UNPACK #-} !DbConnNum
  } deriving (Eq, Read, Show)

mkDbEnv
  :: (MonadBaseControl IO m, MonadIO m)
  => DbConf -> m DbEnv
mkDbEnv dbConf@(DbConf (DbConnStr connStr) (DbConnNum connNum)) = do
  connPool <- runStdoutLoggingT $ createPostgresqlPool connStr connNum
  pure $ DbEnv {dbEnvPool = connPool, dbEnvConf = dbConf}

data ServerEnv = ServerEnv
  { serverEnvConf :: !ServerConf
  , serverEnvHttpManager :: !HttpManager
  , serverEnvMiddleware :: !Middleware
  }

data ServerConf = ServerConf
  { serverConfEnvironment :: {-# UNPACK #-} !Environment
  , serverConfPort :: {-# UNPACK #-} !Port
  }

-- TODO: Try to create Lens' for all of the Env and conf types.

data AppEnv = AppEnv
  { appEnvServer :: ServerEnv
  , appEnvDb :: DbEnv
  }

data AppConf = AppConf
  { appConfServer :: ServerConf
  , appConfDb :: DbConf
  }

class Conf2Env conf env | conf -> env, env -> conf where
  conf2Env :: MonadIO m => conf -> m env

  mkEnv :: (FromEnv conf, MonadIO m) => m env
  mkEnv = do
    eitherConf <- liftIO decodeEnv
    case eitherConf of
      Left err -> undefined
      Right conf -> conf2Env conf

instance Conf2Env DbConf DbEnv where
  conf2Env :: MonadIO m => DbConf -> m DbEnv
  conf2Env dbConf@(DbConf (DbConnStr connStr) (DbConnNum connNum)) = do
    connPool <- runStdoutLoggingT $ createPostgresqlPool connStr connNum
    pure $ DbEnv {dbEnvPool = connPool, dbEnvConf = dbConf}

instance Conf2Env ServerConf ServerEnv where
  conf2Env :: MonadIO m => ServerConf -> m ServerEnv
  conf2Env serverConf = do
    manager <- newHttpManager
    pure $
      ServerEnv
        { serverEnvConf = serverConf
        , serverEnvHttpManager = manager
        , serverEnvMiddleware =
            reqLoggerMiddleware $ serverConfEnvironment serverconf
        }

instance Conf2Env AppConf AppEnv where
  conf2Env :: MonadIO m => AppConf -> m AppEnv
  conf2Env (AppConf serverConf dbConf) =
    AppEnv <$> conf2Env serverConf <*> conf2Env dbConf

class HasMiddleware a where
  middlewareLens :: Lens' a Middleware

instance HasMiddleware AppEnv where
  middlewareLens :: Lens' AppEnv Middleware
  middlewareLens = lens (appConfServer

reqLogMiddleware :: Environment -> Middleware
reqLogMiddleware Production = logStdout
reqLogMiddleware Test = id
reqLogMiddleware _ = logStdoutDev

type ServerM = ReaderT ServerEnv IO

type DbT m = ReaderT SqlBackend m

type Api = ApiFooBar :<|> ApiHogeHoge

type ApiFooBar =
  "foobar" :>
  Post '[JSON] FooBarResp

type ApiHogeHoge =
  "hogehoge" :>
  Get '[JSON] HogeHogeResp

data FooBarResp = FooBarResp Int

data HogeHogeResp = HogeHogeResp Text

serverRoot :: ServerT Api AppM
serverRoot = postFooBar :<|> getHogeHoge

postFooBar :: AppM FooBarResp
postFooBar = pure $ FooBarResp 1

getHogeHoge :: AppM HogeHogeResp
getHogeHoge = pure $ HogeHogeResp "hogehoge"

-- | Create a WAI 'Application'.
app :: Application
app = serve (Proxy :: Proxy Api) serverRoot

setup :: IO ServerEnv
setup = do
  eitherConfg <- decodeEnv
  case eitherConfg of
    Left err ->
      error . unlines $ ["Error: Environment variables are not expected", err]
    Right cfg -> pure cfg

-- | Write this function in terms of Conf2Env functions and Has* functions.
runApp :: IO ()
runApp = do
  cfg <- mkEnv
  let middleware = serverEnvMiddleware cfg
  -- onStartup cfg
  let port = serverConfPort $ serverEnvConf cfg
  putStrLn $ "app running on port " <> show port <> "..."
  Warp.run port . middleware $ app cfg

-- | Run the WAI 'Application' using 'run' on the port defined by 'port'.
main :: IO ()
main = runApp
