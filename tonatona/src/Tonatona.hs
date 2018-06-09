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
  cfg <- setup
  let reqLogMiddleware = getReqLogMiddleware cfg
  onStartup cfg
  let port = getPort cfg
  putStrLn $ "app running on port " <> show port <> "..."
  Warp.run port . reqLogMiddleware $ app cfg

{-| A type class for configuration.
 - The 'config' is supposed to be an instance of 'FromEnv'.
 -}
class (FromEnv config) => TonaConfig config where
  getPort :: config -> Int
  getPort _ = 8000

  getEnv :: config -> Environment
  getEnv _ = Development

  onStartup :: config -> IO ()
  onStartup _ = pure ()

  getReqLogMiddleware :: config -> Middleware
  getReqLogMiddleware conf =
    case getEnv conf of
      Production -> logStdout
      Test -> id
      _ -> logStdoutDev

data Environment
  = Development
  | Production
  | Staging
  | Test
  deriving (Eq, Show, Read)

setup :: TonaConfig config => IO config
setup = do
  eitherConfg <- decodeEnv
  case eitherConfg of
    Left err ->
      error . unlines $ ["Error: Environment variables are not expected", err]
    Right cfg -> pure cfg
