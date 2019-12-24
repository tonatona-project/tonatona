module Tonatona
  ( run
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

{-| Lift Continuation-passing style IO function into RIO.
 -}
liftIOCont :: ((a -> IO b) -> IO c) -> (a -> RIO env b) -> RIO env c
liftIOCont f action =
  RIO $ ReaderT $ \env -> f (\a -> runReaderT (unRIO (action a)) env)

class HasParser a where
  parser :: Parser a

class HasConfig env config where
  config :: env -> config
