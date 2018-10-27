module Tonatona
  ( run
  , runWithConf
  , runWithConfAndShared
  , asksConf
  , asksShared
  , askConf
  , askShared
  , TonaM
  , Plug(..)
  ) where

import RIO

import TonaParser (FromEnv, decodeEnv)

{-| Main type
 -}
type TonaM conf shared
   = RIO (conf, shared)

{-| Main function.
 -}
run :: Plug conf shared => TonaM conf shared a -> IO a
run action = do
  mconf <- decodeEnv
  case mconf of
    Nothing -> error "Fail to decode env"
    Just conf -> runWithConf conf action

runWithConf :: Plug conf shared => conf -> TonaM conf shared a -> IO a
runWithConf conf action = do
  shared <- Tonatona.init conf
  runWithConfAndShared conf shared action

runWithConfAndShared :: conf -> shared -> TonaM conf shared a -> IO a
runWithConfAndShared conf shared action = runRIO (conf, shared) action

asksConf :: (conf -> a) -> TonaM conf shared a
asksConf f = asks (f . fst)

asksShared :: (shared -> a) -> TonaM conf shared a
asksShared f = asks (f . snd)

askConf :: TonaM conf shared conf
askConf = asksConf id

askShared :: TonaM conf shared shared
askShared = asksShared id

{-| A type class for configuration.
 - The 'config' is supposed to be an instance of 'FromEnv'.
 -}
class (FromEnv conf) => Plug conf share | conf -> share, share -> conf where
  init :: conf -> IO share
