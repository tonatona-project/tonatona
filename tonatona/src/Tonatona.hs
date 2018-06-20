{-# LANGUAGE FunctionalDependencies #-}

module Tonatona
  ( run
  , runWithConf
  , runWithConfAndShared
  , readerConf
  , readerShared
  , askConf
  , askShared
  , TonaM
  , Plug(..)
  , lift
  ) where

import Control.Monad.Reader (ReaderT, runReaderT, reader)
import Control.Monad.Trans (lift)
import Data.Semigroup ((<>))
import System.Envy (FromEnv, decodeEnv)

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
    Right conf -> runWithConf conf action

runWithConf :: Plug conf shared => conf -> TonaM conf shared a -> IO a
runWithConf conf action = do
  shared <- Tonatona.init conf
  runWithConfAndShared conf shared action

runWithConfAndShared :: conf -> shared -> TonaM conf shared a -> IO a
runWithConfAndShared conf shared action = runReaderT action (conf, shared)

readerConf :: (conf -> a) -> TonaM conf shared a
readerConf f = reader (f . fst)

readerShared :: (shared -> a) -> TonaM conf shared a
readerShared f = reader (f . snd)

askConf :: TonaM conf shared conf
askConf = readerConf id

askShared :: TonaM conf shared shared
askShared = readerShared id

{-| A type class for configuration.
 - The 'config' is supposed to be an instance of 'FromEnv'.
 -}
class (FromEnv conf) => Plug conf share | conf -> share, share -> conf where
  init :: conf -> IO share
