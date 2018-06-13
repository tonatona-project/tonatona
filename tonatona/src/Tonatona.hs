{-# LANGUAGE FunctionalDependencies #-}

module Tonatona
  ( run
  , TonaM
  , Plug(..)
  , lift
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Data.Semigroup ((<>))
import System.Envy (FromEnv, decodeEnv)
import qualified System.Envy as Envy

{-| Main type
 - TODO make this an opaque type, and appropreate Monad instead of `IO`
 -}
type TonaM conf shared
   = ReaderT (conf, shared) IO

{-| Main function.
 -}
run :: Plug conf shared => TonaM conf shared a -> IO a
run action = do
  mconf <- decodeEnv
  case mconf of
    Left err -> error $ "Fail to decode env: " <> err
    Right conf -> do
      shared <- Tonatona.init conf
      runReaderT action (conf, shared)

{-| A type class for configuration.
 - The 'config' is supposed to be an instance of 'FromEnv'.
 -}
class (FromEnv conf) => Plug conf share | conf -> share, share -> conf where
  init :: conf -> IO share
