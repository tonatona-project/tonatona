module Tonatona
  ( run
  , TonaConfig(..)
  , Environment(..)
  ) where

import Data.Semigroup ((<>))
import Network.Wai (Application, Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.Envy (FromEnv, decodeEnv)

{-| Main function.
 - By providing your 'Application', this function adds several features for handling annoying real word tasks.
 -}
run :: TonaConfig config => (config -> Application) -> IO ()
run app = do
  (cfg, requestLoggerMiddleware) <- setup
  onStartup cfg
  let port = getPort cfg
  putStrLn $ "goatass running on port " <> show port <> "..."
  Warp.run port . requestLoggerMiddleware $ app cfg

{-| A type class for configuration.
 - The 'config' is supposed to be an instance of 'FromEnv'.
 -}
class (FromEnv config) =>
      TonaConfig config
  where
  getPort :: config -> Int
  getEnv :: config -> Environment
  onStartup :: config -> IO ()

{-| TODO: Support 'Staging' environment and other custom values.
 -}
data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

setup :: TonaConfig config => IO (config, Middleware)
setup = do
  eitherConfg <- decodeEnv
  case eitherConfg of
    Left err ->
      error . unlines $ ["Error: Environment variables are not expected", err]
    Right cfg -> pure (cfg, getRequestLoggerMiddleware $ getEnv cfg)

getRequestLoggerMiddleware :: Environment -> Middleware
getRequestLoggerMiddleware Test = id
getRequestLoggerMiddleware Development = logStdoutDev
getRequestLoggerMiddleware Production = logStdout
