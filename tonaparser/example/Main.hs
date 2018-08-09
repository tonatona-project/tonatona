module Main where

import Data.Semigroup ((<>))

import TonaParser
  ( FromEnv(fromEnv)
  , Parser
  , ParserAlts(..)
  , ParserMods(..)
  , (.||)
  , argLong
  , argShort
  , decodeEnvWith
  , defParserAlts
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
  let alts =
        defParserAlts
          { cmdLineLongAlts = [("foo", "new-foo")]
          , cmdLineShortAlts = [('b', 'c')]
          , envVarAlts = [("BAR_BAZ", "NEW_BAR_BAZ")]
          }
  (foo :: Maybe Foo) <- decodeEnvWith alts defParserMods
  print foo
