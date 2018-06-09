module Tonatona
  ( run
  , Config(..)
  , TonaM
  , TonaConfig(..)
  , Environment(..)
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Semigroup ((<>))
import GHC.Generics (Generic)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.Envy (FromEnv(..), (.!=), envMaybe)
import qualified System.Envy as Envy
import Text.Read (readMaybe)

{-| Main type
 - TODO make this an opaque type, and appropreate Monad instead of `IO`
 -}
type TonaM conf a
   = (TonaConfig conf) =>
       ReaderT conf IO a

{-| Main function.
 -}
run :: TonaConfig conf => TonaM conf a -> IO a
run ma = do
  mconf <- Envy.decode
  case mconf of
    Nothing -> error "Fail to decode env"
    Just conf -> do
      putStrLn $ "Running as " <> show (environment . getConfig $ conf)
      runReaderT ma conf

data Config = Config
  { environment :: Environment
  }
  deriving (Show)


instance FromEnv Config where
  fromEnv = Config
    <$> envMaybe "ENV" .!= Development

{-| A type class for configuration.
 - The 'config' is supposed to be an instance of 'FromEnv'.
 -}
class (FromEnv config) =>
      TonaConfig config
  where
  getConfig :: config -> Config
  getReqLogMiddleware :: config -> Middleware
  getReqLogMiddleware conf =
    case environment (getConfig conf) of
      Production -> logStdout
      Test -> id
      _ -> logStdoutDev

data Environment
  = Development
  | Production
  | Staging
  | Test
  deriving (Eq, Generic, Show, Read)

instance Envy.Var Environment where
  toVar = show
  fromVar = readMaybe
