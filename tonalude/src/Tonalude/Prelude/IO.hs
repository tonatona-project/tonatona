module Tonalude.Prelude.IO
  ( withLazyFile
  , readFileBinary
  , writeFileBinary
  , readFileUtf8
  , writeFileUtf8
  , hPutBuilder
  ) where

import RIO hiding
  ( withLazyFile
  , readFileBinary
  , writeFileBinary
  , readFileUtf8
  , writeFileUtf8
  , hPutBuilder
  )
import qualified RIO

import qualified Data.ByteString as B ()
import qualified Data.ByteString.Lazy as BL

-- | Lazily get the contents of a file. Unlike 'BL.readFile', this
-- ensures that if an exception is thrown, the file handle is closed
-- immediately.
withLazyFile :: FilePath -> (BL.ByteString -> RIO env a) -> RIO env a
withLazyFile = RIO.withLazyFile

-- | Write a file in UTF8 encoding
--
-- This function will use OS-specific line ending handling.
writeFileUtf8 :: FilePath -> Text -> RIO env ()
writeFileUtf8 = RIO.writeFileUtf8

hPutBuilder :: Handle -> Builder -> RIO env ()
hPutBuilder = RIO.hPutBuilder

-- | Same as 'B.readFile', but focused on 'RIO'
readFileBinary :: FilePath -> RIO env ByteString
readFileBinary = RIO.readFileBinary

-- | Same as 'B.writeFile', but focused on 'RIO'
writeFileBinary :: FilePath -> ByteString -> RIO env ()
writeFileBinary = RIO.writeFileBinary

-- | Read a file in UTF8 encoding, throwing an exception on invalid character
-- encoding.
--
-- This function will use OS-specific line ending handling.
readFileUtf8 :: FilePath -> RIO env Text
readFileUtf8 = RIO.readFileUtf8
