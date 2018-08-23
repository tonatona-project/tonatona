module Main where

import Data.Semigroup ((<>))

import TonaParser
  ( FromEnv(fromEnv)
  , Parser
  , ParserRenames(..)
  , ParserMods(..)
  , (.||)
  , argLong
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
    <$> envDef (argLong "baz" .|| envVar "BAZ") "baz"

barWithPrefix :: Parser Bar
barWithPrefix =
  fromEnvWith
    defParserRenames
    defParserMods
      { cmdLineLongMods = ("bar-" <>)
      , envVarMods = ("BAR_" <>)
      }

instance FromEnv Foo where
  fromEnv = Foo
    <$> env (argLong "foo" .|| envVar "FOO")
    <*> barWithPrefix

main :: IO ()
main = do
  let renames =
        defParserRenames
          { cmdLineLongRenames = [("foo", "new-foo")]
          , envVarRenames = [("BAR_BAZ", "NEW_BAR_BAZ")]
          }
  (foo :: Maybe Foo) <- decodeEnvWith renames defParserMods
  print foo
