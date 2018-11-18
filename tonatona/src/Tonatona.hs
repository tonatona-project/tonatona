module Tonatona
  ( run
  , HasParser(..)
  , HasConfig(..)
  ) where

import RIO

import TonaParser (Parser, withConfig)

{-| Main function.
 -}
run :: HasParser a env => RIO env a -> IO a
run action = do
  withConfig parser $ \env ->
    runRIO env action

class HasParser r a where
  parser :: Parser r a

class HasConfig env config where
  config :: env -> config
