module TonaParser
  (
  -- * Run parser
    Parser (..)
  , withConfig
  -- * Construct primitive parsers
  , optionalVal
  , requiredVal
  , Source
  , module System.Envy
  , Description
  , (.||)
  , envVar
  , argLong
  -- * Modify parsers
  , modify
  , defParserMods
  , ParserMods
  , cmdLineLongMods
  , envVarMods
  ) where

{-| Integrated parser library created for tonatona meta application framework.
 - It can construct system configuration from environment variables, command line arguments, and any IO values depends on them.
 - See details for @example/Main.hs@.
-}

import RIO
import qualified RIO.Map as Map

import Control.Monad (ap)
import System.Environment (getArgs, getEnvironment)
import System.Envy (Var(fromVar, toVar))



-- Types

{-| Main type representing how to construct system configuration.
 -}
newtype Parser r a = Parser
  { runParser :: Config -> (a -> IO r) -> IO r }

instance Functor (Parser r) where
  fmap :: (a -> b) -> Parser r a -> Parser r b
  fmap f p = Parser $ \conf action ->
    runParser p conf (action . f)

instance Applicative (Parser r) where
  pure :: a -> Parser r a
  pure a = Parser $ \_ -> ($ a)

  (<*>) = ap

instance Monad (Parser r) where
  (>>=) :: Parser r a -> (a -> Parser r b) -> Parser r b
  p >>= k = Parser $ \conf action ->
    runParser p conf (\x -> runParser (k x) conf (action))

instance MonadIO (Parser r) where
  liftIO :: forall a. IO a -> Parser r a
  liftIO ma = Parser $ \_ action -> action =<< ma


-- Operators


modify :: ParserMods -> Parser r a -> Parser r a
modify mods (Parser parserFunc) =
  Parser $ \oldConfig ->
    let newConfig =
          oldConfig
            { confParserMods = confParserMods oldConfig <> mods
            }
    in parserFunc newConfig

withConfig :: Parser r a -> (a -> IO r) -> IO r
withConfig parser action = do
  envVars <- getEnvVars
  cmdLineArgs <- getCmdLineArgs
  parse parser envVars cmdLineArgs action

parse ::
     Parser r a
  -> Map String String -- ^ Environment variables.
  -> [(String, String)] -- ^ Command line arguments and values.
  -> (a -> IO r)
  -> IO r
parse (Parser parserFunc) envVars cmdLineArgs =
  let conf =
        Config
          { confCmdLineArgs = cmdLineArgs
          , confEnvVars = envVars
          , confParserMods = defParserMods
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

requiredVal :: Var a => Description -> Source -> Parser r a
requiredVal desc srcs = do
  ma <- fieldMaybe desc srcs
  case ma of
    Just a -> pure a
    Nothing ->
      Parser $ \_ _ -> error $ "No required configuration for \"" <> unDescription desc <> "\""

optionalVal :: Var a => Description -> Source -> a -> Parser r a
optionalVal desc srcs df = do
  ma <- fieldMaybe desc srcs
  case ma of
    Nothing -> pure df
    Just a -> pure a

fieldMaybe :: Var a => Description -> Source -> Parser r (Maybe a)
fieldMaybe _desc (Source srcs) =
  Parser $ \conf action -> do
    -- find matching source
    let mval = findValInSrc conf srcs
    action (fromVar =<< mval)



findValInSrc :: Config -> [InnerSource] -> Maybe String
findValInSrc conf srcs = listToMaybe $ mapMaybe (findValInSrcs conf) srcs

findValInSrcs :: Config -> InnerSource -> Maybe String
findValInSrcs conf innerSource =
  let cmdLineArgs = confCmdLineArgs conf
      envVars = confEnvVars conf
      mods = confParserMods conf
      longMods = cmdLineLongMods mods
      shortMods = cmdLineShortMods mods
      envMods = envVarMods mods
  in
  case innerSource of
    ArgLong str -> findValInCmdLineLong cmdLineArgs longMods str
    ArgShort ch -> findValInCmdLineShort cmdLineArgs shortMods ch
    EnvVar var -> findValInEnvVar envVars envMods var

findValInCmdLineLong
  :: [(String, String)]
  -> (String -> String)
  -> String
  -> Maybe String
findValInCmdLineLong args modFunc str =
  let modifiedVal = modFunc str
  in lookup modifiedVal args

findValInCmdLineShort
  :: [(String, String)]
  -> (Char -> Char)
  -> Char
  -> Maybe String
findValInCmdLineShort args modFunc ch =
  let modifiedVal = modFunc ch
  in lookup [modifiedVal] args

findValInEnvVar
  :: Map String String
  -> (String -> String)
  -> String
  -> Maybe String
findValInEnvVar args modFunc var =
  let modifiedVal = modFunc var
  in Map.lookup modifiedVal args

data Config = Config
  { confCmdLineArgs :: [(String, String)]
  , confEnvVars :: Map String String
  , confParserMods :: ParserMods
  }

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

newtype Source = Source { _unSource :: [InnerSource] }

(.||) :: Source -> Source -> Source
(.||) (Source a) (Source b) = Source (a ++ b)

newtype Description = Description { unDescription :: String }
  deriving (Show, Eq, Read, IsString)

envVar :: String -> Source
envVar name =
  Source [EnvVar name]

argLong :: String -> Source
argLong name = Source [ArgLong name]

-- argShort :: Char -> Source
-- argShort name = Source [ArgShort name]
