module Tonatona.IO
  ( run
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Tonatona (TonaM)

{-| Main function.
 -}
run :: (conf -> shared -> IO a) -> TonaM conf shared a
run f = do
  (conf, shared) <- ask
  liftIO $ f conf shared
