module TonaParser
  (..) where

import Control.Monad.Reader (ReaderT, runReaderT, reader)
import Control.Monad.Trans (lift)
import Data.Semigroup ((<>))
import System.Envy (FromEnv, decodeEnv)

{-| Example

  data Foo = Foo
    { foo :: Int
    , bar :: Bar
    }

  data Bar = Bar
    { baz :: String
    }

  -- If environment variable "BAZ" exist, use the value
  -- else if command line argument "--baz" exist, use the value
  -- else if command line argument "-b" exist, use the value
  -- else use default value "baz"
  FromEnv Bar where
    fromEnv = Bar
      <$> envMaybe (envVar "BAZ" .|| argLong "baz" .|| argShort 'b') .!= "baz"

  -- If environment variable "BAR_BAZ" exist, use the value
  -- else if command line argument "--bar-baz" exist, use the value
  -- else use default value "baz"
  -- ( short hands argument "-b" was deactivated )
  barWithPrefix :: Parser a
  barWithPrefix =
    modifyConfig $
      modifyEnvVarKey ("BAR_" <>) .
      modifyArgLongKey ("bar-" <>) .
      modifyArgShortKey const Nothing

  FromEnv Foo where
    fromEnv = Foo
      <$> env (envVar "FOO" .|| argLong "foo")
      <*> barWithPrefix
 -}



class FromEnv a where
  fromEnv :: Parser a

newtype Parser a = Parser
  { getConfig :: Config a
  } deriving ( Functor
             , Monad
             , Applicative
             , MonadError String
             , MonadIO
             , Alternative
             , MonadPlus
             )


runParser :: ExceptT String IO a
runParser = undefined


{-| Opaque type.
 -}
data Config a = Config
  { defaultValue :: Maybe a
  , sources :: [InnerSource]
  }


data InnerSource
  = EnvVar String
  | ArgLong String
  | ArgShort Char


newtype Source = Source { unSource :: [InnerSource] }


(.||) :: Source -> Source -> Source
(.||) (Source a) (Source b) = Source (a ++ b)


envVar :: String -> Source
envVar name =
  Source [EnvVar name]
