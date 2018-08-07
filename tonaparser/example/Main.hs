module Main where

import TonaParser (FromEnv(fromEnv), (.||), argLong, argShort, decodeEnv, envDef, envVar)

data Bar = Bar
  { baz :: String
  } deriving Show

-- If environment variable "BAZ" exist, use the value
-- else if command line argument "--baz" exist, use the value
-- else if command line argument "-b" exist, use the value
-- else use default value "baz"
instance FromEnv Bar where
  fromEnv = Bar
    <$> envDef (envVar "BAZ" .|| argLong "baz" .|| argShort 'b') "baz"

main :: IO ()
main = do
  (bar :: Maybe Bar) <- decodeEnv
  print bar
