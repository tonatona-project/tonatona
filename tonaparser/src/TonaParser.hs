{-| Integrated parser library created for tonatona meta application framework.
  It can construct system configuration from environment variables, command line arguments, and any IO values depends on them.
  See details for @example/Main.hs@.
-}

module TonaParser
  (
  -- * Run parser
    Parser
  , withConfig
  -- * Construct primitive parsers
  , optionalVal
  , requiredVal
  , liftWith
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

import RIO
import qualified RIO.Map as Map

import Control.Monad (ap)
import Data.Typeable (Proxy(..), typeOf, typeRep)
import Say (sayString)
import System.Environment (getArgs, getEnvironment)
import System.Envy (Var(fromVar, toVar))



-- Types

{-| Main type representing how to construct system configuration.
 -}
newtype Parser a = Parser
  { runParser :: Bool -> Config -> (Bool -> a -> IO ()) -> IO () }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \b conf action ->
    runParser p b conf (\b' -> action b' . f)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \b _ -> (\action -> action b a)

  (<*>) = ap

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= k = Parser $ \b conf action ->
    runParser p b conf $ \b' x ->
      runParser (k x) b' conf action

instance MonadIO Parser where
  liftIO :: forall a. IO a -> Parser a
  liftIO ma = Parser $ \b _ action -> action b =<< ma


-- Operators


modify :: ParserMods -> Parser a -> Parser a
modify mods (Parser parserFunc) =
  Parser $ \b oldConfig ->
    let newConfig =
          oldConfig
            { confParserMods = confParserMods oldConfig <> mods
            }
    in parserFunc b newConfig

withConfig :: Parser a -> (a -> IO ()) -> IO ()
withConfig parser action = do
  envVars <- getEnvVars
  cmdLineArgs <- getCmdLineArgs
  args <- getArgs
  let isHelp = length (filter (`elem` ["--help", "-h"]) args) > 0
  parse parser isHelp envVars cmdLineArgs action

parse ::
     Parser a
  -> Bool
  -> Map String String -- ^ Environment variables.
  -> [(String, String)] -- ^ Command line arguments and values.
  -> (a -> IO ())
  -> IO ()
parse (Parser parserFunc) isHelp envVars cmdLineArgs action =
  parserFunc isHelp conf $ \b a ->
    if b
      then do
        sayString $ unlines
          [ "Display this help and exit"
          , "    Default: False"
          , "    Type: Bool"
          , "    Command line option: -h"
          , "    Command line option: --help"
          ]
      else action a
  where
    conf =
        Config
          { confCmdLineArgs = cmdLineArgs
          , confEnvVars = envVars
          , confParserMods = defParserMods
          }


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

{-| A 'Parser' constructor for required values.
-}
requiredVal :: Var a => Description -> Source -> Parser a
requiredVal desc srcs = do
  ma <- fieldMaybe Nothing desc srcs
  case ma of
    Just a -> pure a
    Nothing ->
      Parser $ \_ _ -> error $ "No required configuration for \"" <> unDescription desc <> "\""

{-| A 'Parser' constructor for optional values.
-}
optionalVal :: Var a => Description -> Source -> a -> Parser a
optionalVal desc srcs df = do
  ma <- fieldMaybe (Just df) desc srcs
  case ma of
    Nothing -> pure df
    Just a -> pure a

{-| A `Parser` constructor from @cont@.
-}
liftWith :: ((a -> IO ()) -> IO ()) -> Parser a
liftWith cont = Parser $ \b _ action -> cont (action b)

fieldMaybe :: (Var a) => Maybe a -> Description -> Source -> Parser (Maybe a)
fieldMaybe mdef desc (Source srcs) =
  Parser $ \isHelp conf action -> do
    when isHelp $
      sayString $ helpLine mdef (confParserMods conf) desc (Source srcs)
    let mval = findValInSrc conf srcs
    action isHelp (fromVar =<< mval)

helpLine :: forall a. (Var a) => Maybe a -> ParserMods -> Description -> Source -> String
helpLine mdef mods (Description desc) (Source srcs) =
  unlines $
    desc : map (indent 4)
      (helpDefault mdef : helpType (Proxy :: Proxy a) : map (helpSource mods) srcs)

indent :: Int -> String -> String
indent n str =
  replicate n ' ' <> str

helpType :: forall a. Typeable a => Proxy a -> String
helpType p = "Type: " <> case show (typeRep p) of
  "[Char]" -> "String"
  "ByteString" -> "String"
  "Text" -> "String"
  a -> a

helpDefault :: Var a => Maybe a -> String
helpDefault a@Nothing = case show (typeOf a) of
  "Bool" -> "Default: False"
  _ -> "Required"
helpDefault (Just def) = "Default: " <> toVar def

helpSource :: ParserMods -> InnerSource -> String
helpSource ParserMods {envVarMods} (EnvVar str) =
  "Environment variable: " <> envVarMods str
helpSource ParserMods {cmdLineLongMods} (ArgLong str) =
  "Command line option: --" <> cmdLineLongMods str
helpSource ParserMods {cmdLineShortMods} (ArgShort c) =
  "Command line option: -" <> [cmdLineShortMods c]

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
