module Main where

import TonaParser (FromEnv(fromEnv), (.||), argLong, argShort, decodeEnvWithMod, env, envDef, envVar)

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

instance FromEnv Foo where
  fromEnv = Foo
    <$> env (envVar "FOO" .|| argLong "foo")
    <*> fromEnv

main :: IO ()
main = do
  (foo :: Maybe Foo) <-
    decodeEnvWithMod [("BAZ", "NEW_BAZ")] [("foo", "new-foo")] [('b', 'c')]
  print foo
