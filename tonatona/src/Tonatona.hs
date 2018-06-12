{-# LANGUAGE FunctionalDependencies #-}

module Tonatona
  ( run
  , TonaM
  , Plug(..)
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import System.Envy (FromEnv)
import qualified System.Envy as Envy

{-| Main type
 - TODO make this an opaque type, and appropreate Monad instead of `IO`
 -}
type TonaM conf shared
   = ReaderT (conf, shared) IO

{-| Main function.
 -}
run :: Plug conf shared => TonaM conf shared a -> IO a
run ma = do
  mconf <- Envy.decode
  case mconf of
    Nothing -> error "Fail to decode env"
    Just conf -> do
      shared <- Tonatona.init conf
      runReaderT ma (conf, shared)

{-| A type class for configuration.
 - The 'config' is supposed to be an instance of 'FromEnv'.
 -}
class (FromEnv conf) => Plug conf share | conf -> share, share -> conf where
  init :: conf -> IO share
