module TonaParser where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT, reader)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Semigroup ((<>))
import System.Environment (getArgs, getEnvironment)
import System.Envy (Var(fromVar))

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

-- data Foo = Foo
--   { foo :: Int
--   , bar :: Bar
--   }

data Bar = Bar
  { baz :: String
  }

-- If environment variable "BAZ" exist, use the value
-- else if command line argument "--baz" exist, use the value
-- else if command line argument "-b" exist, use the value
-- else use default value "baz"
instance FromEnv Bar where
  fromEnv = Bar
    <$> envDef (envVar "BAZ" .|| argLong "baz" .|| argShort 'b') "baz"

-- If environment variable "BAR_BAZ" exist, use the value
-- else if command line argument "--bar-baz" exist, use the value
-- else use default value "baz"
-- ( short hands argument "-b" was deactivated )
-- barWithPrefix :: Parser a
-- barWithPrefix =
--   modifyConfig $
--     modifyEnvVarKey ("BAR_" <>) .
--     modifyArgLongKey ("bar-" <>) .
--     modifyArgShortKey const Nothing

-- FromEnv Foo where
--   fromEnv = Foo
--     <$> env (envVar "FOO" .|| argLong "foo")
--     <*> barWithPrefix


class FromEnv a where
  fromEnv :: Parser a

decodeEnv :: FromEnv a => IO (Maybe a)
decodeEnv = do
  let parser = fromEnv
  envVars <- getEnvVars
  cmdLineArgs <- getCmdLineArgs
  pure $ runParser parser envVars cmdLineArgs

runParser :: Parser a -> Map String String -> [(String, String)] -> Maybe a
runParser (Parser parserFunc) envVars cmdLineArgs =
  let conf =
        Config
          { confCmdLineArgs = cmdLineArgs
          , confEnvVars = envVars
          }
  in parserFunc conf

getEnvVars :: IO (Map String String)
getEnvVars = do
  environment <- getEnvironment
  pure $ Map.fromList environment

-- TODO: Does not properly handle command line arguments.
-- Probably should use parsing code from something like optparse-applicative.
getCmdLineArgs :: IO [(String, String)]
getCmdLineArgs = do
  args <- getArgs
  pure $ tupleList args
  where
    -- This throws away the last time if the input list is not even.
    tupleList :: [String] -> [(String,String)]
    tupleList (a:b:cs) = (dropWhile (== '-') a, b) : tupleList cs
    tupleList _ = []

env :: Var a => Source -> Parser a
env (Source srcs) =
  Parser $ \conf -> do
    -- find matching source
    val <- findValInSrc conf srcs
    fromVar val

findValInSrc :: Config -> [InnerSource] -> Maybe String
findValInSrc conf srcs = listToMaybe $ mapMaybe (findValInSrcs conf) srcs

findValInSrcs :: Config -> InnerSource -> Maybe String
findValInSrcs (Config cmdLineArgs _) (ArgLong str) = lookup str cmdLineArgs
findValInSrcs (Config cmdLineArgs _) (ArgShort ch) = lookup [ch] cmdLineArgs
findValInSrcs (Config _ envVars) (EnvVar var) = Map.lookup var envVars

envDef :: Var a => Source -> a -> Parser a
envDef src df = Parser $ \conf ->
  let Parser f = env src
  in
  case f conf of
    Nothing -> Just df
    Just a -> Just a

newtype Parser a = Parser
  { getConfig :: Config -> Maybe a
  }
  deriving (Functor
             -- , Monad
             -- , Applicative
             -- , MonadError String
             -- , MonadIO
             -- , Alternative
             -- , MonadPlus
             )


{-| Opaque type.
 -}
data Config = Config
  { confCmdLineArgs :: [(String, String)]
  , confEnvVars :: Map String String
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

argLong :: String -> Source
argLong name = Source [ArgLong name]

argShort :: Char -> Source
argShort name = Source [ArgShort name]
