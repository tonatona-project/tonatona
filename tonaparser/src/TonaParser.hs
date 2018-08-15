{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TonaParser where

import Control.Monad (ap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Semigroup (Semigroup, (<>))
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

  data Foo = Foo
    { foo :: Int
    , bar :: Bar
    }

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

decodeEnv :: FromEnv a => IO (Maybe a)
decodeEnv = decodeEnvWith defParserRenames defParserMods

-- | This function is just like 'decodeEnv', but it provides a way to give
-- alternatives for names environment variables and command line arguments.
decodeEnvWith
  :: FromEnv a
  => ParserRenames
  -> ParserMods
  -> IO (Maybe a)
decodeEnvWith parserRenames parserMods = do
  envVars <- getEnvVars
  cmdLineArgs <- getCmdLineArgs
  pure $ runParser fromEnv envVars cmdLineArgs parserRenames parserMods

runParser
  :: Parser a
  -> Map String String  -- ^ Environment variables.
  -> [(String, String)]  -- ^ Command line arguments and values.
  -> ParserRenames
  -> ParserMods
  -> Maybe a
runParser parser envVars cmdLineArgs renames mods =
  let Parser parserFunc = parser
      conf =
        Config
          { confCmdLineArgs = cmdLineArgs
          , confEnvVars = envVars
          , confParserRenames = renames
          , confParserMods = mods
          }
  in parserFunc conf

modifyParser :: Parser a -> ParserMods -> Parser a
modifyParser (Parser parserFunc) parserMods = Parser $ \conf ->
  parserFunc $
    conf
      { confParserMods = confParserMods conf <> parserMods
      }

fromEnvWith :: FromEnv a => ParserMods -> Parser a
fromEnvWith = modifyParser fromEnv

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
      envVars = confEnvVars conf
      renames = confParserRenames conf
      mods = confParserMods conf
      longRenames = cmdLineLongRenames renames
      shortRenames = cmdLineShortRenames renames
      envRenames = envVarRenames renames
      longMods = cmdLineLongMods mods
      shortMods = cmdLineShortMods mods
      envMods = envVarMods mods
  in
  case innerSource of
    ArgLong str -> findValInCmdLineLong cmdLineArgs longRenames longMods str
    ArgShort ch -> findValInCmdLineShort cmdLineArgs shortRenames shortMods ch
    EnvVar var -> findValInEnvVar envVars envRenames envMods var

findValInCmdLineLong
  :: [(String, String)]
  -> [(String, String)]
  -> (String -> String)
  -> String
  -> Maybe String
findValInCmdLineLong args renames modFunc str =
  let modifiedVal = modFunc str
      valToLookup = lookupDef modifiedVal renames modifiedVal
  in lookup valToLookup args

findValInCmdLineShort
  :: [(String, String)]
  -> [(Char, Char)]
  -> (Char -> Char)
  -> Char
  -> Maybe String
findValInCmdLineShort args renames modFunc ch =
  let modifiedVal = modFunc ch
      valToLookup = lookupDef modifiedVal renames modifiedVal
  in lookup [valToLookup] args

findValInEnvVar
  :: Map String String
  -> [(String, String)]
  -> (String -> String)
  -> String
  -> Maybe String
findValInEnvVar args renames modFunc var =
  let modifiedVal = modFunc var
      valToLookup = lookupDef modifiedVal renames modifiedVal
  in Map.lookup valToLookup args

lookupDef :: Eq a => a -> [(a, b)] -> b -> b
lookupDef a pairs b = fromMaybe b $ lookup a pairs

envDef :: Var a => Source -> a -> Parser a
envDef src df = Parser $ \conf ->
  case (unParser $ env src) conf of
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
  pure a = Parser $ \_ -> Just a

  (<*>) = ap

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser func >>= f = Parser $ \conf ->
    let maybeA = func conf
    in
    case maybeA of
      Nothing -> Nothing
      Just a -> unParser (f a) conf

data Config = Config
  { confCmdLineArgs :: [(String, String)]
  , confEnvVars :: Map String String
  , confParserRenames :: ParserRenames
  , confParserMods :: ParserMods
  }

data ParserRenames = ParserRenames
  { cmdLineLongRenames :: [(String, String)]
  , cmdLineShortRenames :: [(Char, Char)]
  , envVarRenames :: [(String, String)]
  }

defParserRenames :: ParserRenames
defParserRenames = ParserRenames [] [] []

data ParserMods = ParserMods
  { cmdLineLongMods :: String -> String
  , cmdLineShortMods :: Char -> Char
  , envVarMods :: String -> String
  }

instance Semigroup ParserMods where
  ParserMods a b c <> ParserMods a' b' c' =
    ParserMods (a' . a) (b' . b) (c' . c)

instance Monoid ParserMods where
  mappend = (<>)
  mempty = ParserMods id id id

defParserMods :: ParserMods
defParserMods = mempty

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
