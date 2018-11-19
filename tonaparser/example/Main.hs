module Main where

import RIO

import Say (sayShow)
import TonaParser
  ( Parser
  , ParserMods(..)
  , (.||)
  , argLong
  , defParserMods
  , envVar
  , modify
  , optionalVal
  , requiredVal
  , withConfig
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
-- else use default value "baz"
barParser :: Parser Bar
barParser =
  Bar <$>
    optionalVal
      "Configuration for Bar.baz"
      (argLong "baz" .|| envVar "BAZ")
      "baz"

setPrefixBar :: Parser Bar -> Parser Bar
setPrefixBar =
  modify
    defParserMods
      { cmdLineLongMods = ("bar-" <>)
      , envVarMods = ("BAR_" <>)
      }

fooParser :: Parser Foo
fooParser = Foo
  <$> requiredVal
    "Configuration for Foo.foo"
    (argLong "foo" .|| envVar "FOO")
  <*> setPrefixBar barParser

modifyFoo :: Parser Foo -> Parser Foo
modifyFoo =
  modify
    defParserMods
      { cmdLineLongMods = longMods
      , envVarMods = envMods
      }
  where
    longMods "foo" = "new-foo"
    longMods a = a
    envMods "BAR_BAZ" = "NEW_BAR_BAZ"
    envMods a = a

main :: IO ()
main =
  withConfig (modifyFoo fooParser) $ \config ->
    sayShow $ config
