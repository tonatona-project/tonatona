{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TonaParser where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, ap)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT, reader)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
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
decodeEnv = decodeEnvWithMod [] [] []

-- | This function is just like 'decodeEnv', but it provides a way to give
-- alternatives for names environment variables and command line arguments.
decodeEnvWithMod
  :: FromEnv a
  => [(String, String)]  -- ^ Alternatives for environment variables.
  -> [(String, String)]  -- ^ Alternatvies for long command line flags.
  -> [(Char, Char)]      -- ^ Alternatives for short command line flags
  -> IO (Maybe a)
decodeEnvWithMod envVarAlts cmdLineArgLongAlts cmdLineArgShortAlts = do
  let parser = fromEnv
  envVars <- getEnvVars
  cmdLineArgs <- getCmdLineArgs
  pure $
    runParser
      parser
      envVars
      cmdLineArgs
      envVarAlts
      cmdLineArgLongAlts
      cmdLineArgShortAlts

runParser
  :: Parser a
  -> Map String String
  -> [(String, String)]  -- ^ Actual command line arguments and values.
  -> [(String, String)]  -- ^ Alternatives for environment variables.
  -> [(String, String)]  -- ^ Alternatvies for long command line flags.
  -> [(Char, Char)]  -- ^ Alternatvies for short command line flags.
  -> Maybe a
runParser parser envVars cmdLineArgs envVarAlts cmdLineLongAlts cmdLineShortAlts =
  let Parser parserFunc = parser
      conf =
        Config
          { confCmdLineArgs = cmdLineArgs
          , confCmdLineLongAlts = cmdLineLongAlts
          , confCmdLineShortAlts = cmdLineShortAlts
          , confEnvVarAlts = envVarAlts
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
findValInSrcs conf innerSource =
  let cmdLineArgs = confCmdLineArgs conf
      cmdLineLongAlts = confCmdLineLongAlts conf
      cmdLineShortAlts = confCmdLineShortAlts conf
      envVars = confEnvVars conf
      envVarAlts = confEnvVarAlts conf
  in
  case innerSource of
    ArgLong str -> findValInCmdLineLong cmdLineArgs cmdLineLongAlts str
    ArgShort ch -> findValInCmdLineShort cmdLineArgs cmdLineShortAlts ch
    EnvVar var -> findValInEnvVar envVars envVarAlts var

findValInCmdLineLong
  :: [(String, String)] -> [(String, String)] -> String -> Maybe String
findValInCmdLineLong args alts str =
  let valToLookup = fromMaybe str $ lookup str alts
  in lookup valToLookup args

findValInCmdLineShort
  :: [(String, String)] -> [(Char, Char)] -> Char -> Maybe String
findValInCmdLineShort args alts ch =
  let valToLookup = fromMaybe ch $ lookup ch alts
  in lookup [valToLookup] args

findValInEnvVar
  :: Map String String -> [(String, String)] -> String -> Maybe String
findValInEnvVar args alts var =
  let valToLookup = fromMaybe var $ lookup var alts
  in Map.lookup valToLookup args

envDef :: Var a => Source -> a -> Parser a
envDef src df = Parser $ \conf ->
  let Parser f = env src
  in
  case f conf of
    Nothing -> Just df
    Just a -> Just a

newtype Parser a = Parser
  { unParser :: Config -> Maybe a
  }
  deriving (Functor
             -- , Monad
             -- , MonadError String
             -- , MonadIO
             -- , Alternative
             -- , MonadPlus
             )

instance Applicative Parser where
  pure a = Parser $ \conf -> Just a

  (<*>) = ap

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser func >>= f = Parser $ \conf ->
    let maybeA = func conf
    in
    case maybeA of
      Nothing -> Nothing
      Just a -> unParser (f a) conf

{-| Opaque type.
 -}
data Config = Config
  { confCmdLineArgs :: [(String, String)]
  , confCmdLineLongAlts :: [(String, String)]
  , confCmdLineShortAlts :: [(Char, Char)]
  , confEnvVarAlts :: [(String, String)]
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
