module Tonalude.Prelude.Display
  ( Utf8Builder (..)
  , Display (..)
  , displayShow
  , utf8BuilderToText
  , utf8BuilderToLazyText
  , displayBytesUtf8
  , Tonalude.Prelude.Display.writeFileUtf8Builder
  ) where

import RIO


{- | Write the given Utf8Builder value to a file.
-}
writeFileUtf8Builder :: FilePath -> Utf8Builder -> RIO env ()
writeFileUtf8Builder = RIO.writeFileUtf8Builder
