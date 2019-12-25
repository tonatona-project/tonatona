module Tonatona
  ( run
  , liftIOMap
  , liftIOCont
  , HasParser(..)
  , HasConfig(..)
  ) where

import RIO

import TonaParser (Parser, withConfig)

{-| Main function.
 -}
run :: HasParser env => RIO env () -> IO ()
run action = do
  withConfig parser $ \env ->
    runRIO env action

{-| Lift IO map function into RIO.
 -}
liftIOMap :: (IO a -> IO b) -> RIO env a -> RIO env b
liftIOMap f rio =
  RIO $ ReaderT $ \env -> f (runReaderT (unRIO rio) env)

{-| Lift Continuation-passing style IO function into RIO.
 -}
liftIOCont :: ((a -> IO b) -> IO c) -> (a -> RIO env b) -> RIO env c
liftIOCont f action =
  RIO $ ReaderT $ \env -> f (\a -> runReaderT (unRIO (action a)) env)

class HasParser a where
  parser :: Parser a

class HasConfig env config where
  config :: env -> config
