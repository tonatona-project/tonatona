module Main where

import Data.Semigroup ((<>))

import TonaParser
  ( FromEnv(fromEnv)
  , Parser
  , ParserRenames(..)
  , ParserMods(..)
  , (.||)
  , argLong
  , argShort
  , decodeEnvWith
  , defParserRenames
  , defParserMods
  , env
  , envDef
  , envVar
  , fromEnvWith
  )

data Bar = Bar
  { baz :: String
  } deriving Show

data Foo = Foo
  { foo :: Int
  , bar :: Bar
  } deriving Show

-- If environment variable "BAZ" exist, use the value
-- else if command line argument "--baz" exist, use the value
-- else if command line argument "-b" exist, use the value
-- else use default value "baz"
instance FromEnv Bar where
  fromEnv = Bar
    <$> envDef (envVar "BAZ" .|| argLong "baz" .|| argShort 'b') "baz"

barWithPrefix :: Parser Bar
barWithPrefix =
  fromEnvWith $
    defParserMods
      { cmdLineLongMods = [("bar-" <>)]
      , envVarMods = [("BAR_" <>)]
      }

instance FromEnv Foo where
  fromEnv = Foo
    <$> env (envVar "FOO" .|| argLong "foo")
    <*> barWithPrefix

main :: IO ()
main = do
  let renames =
        defParserRenames
          { cmdLineLongRenames = [("foo", "new-foo")]
          , cmdLineShortRenames = [('b', 'c')]
          , envVarRenames = [("BAR_BAZ", "NEW_BAR_BAZ")]
          }
  (foo :: Maybe Foo) <- decodeEnvWith renames defParserMods
  print foo
