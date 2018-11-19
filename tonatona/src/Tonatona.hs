module Tonatona
  ( run
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

class HasParser a where
  parser :: Parser a

class HasConfig env config where
  config :: env -> config
