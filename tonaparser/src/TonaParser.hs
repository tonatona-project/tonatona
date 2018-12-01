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
  , optionalEnum
  , requiredEnum
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
import qualified RIO.List as List
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
  pure a = Parser $ \b _ action -> action b a

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

-- TODO: Handle short-hands options.
getCmdLineArgs :: IO [(String, String)]
getCmdLineArgs = do
  args <- getArgs
  pure $ parseArgs args

{-|
  >>> parseArgs ["--bool", "--foo", "bar", "-v"]
  [ ("bool", ""), ("foo", "bar") ]
-}
parseArgs :: [String] -> [(String, String)]
parseArgs [] = []
parseArgs [('-':'-':key)] = [(key, "")]
parseArgs (('-':'-':key):ls@(('-':_):_)) = (key, "") : parseArgs ls
parseArgs (('-':'-':key):val:ls) = (key, val) : parseArgs ls
parseArgs (('-':_):ls) = parseArgs ls
parseArgs (_:ls) = parseArgs ls

{-| A 'Parser' constructor for required values.
-}
requiredVal :: Var a => Description -> Source -> Parser a
requiredVal desc srcs = do
  ma <- fieldMaybe Nothing desc srcs
  handleRequired desc ma

{-| A 'Parser' constructor for optional values.
-}
optionalVal :: Var a => Description -> Source -> a -> Parser a
optionalVal desc srcs df = do
  ma <- fieldMaybe (Just df) desc srcs
  maybe (pure df) pure ma

{-| A 'Parser' constructor for required values.
-}
requiredEnum :: (Var a, Enum a, Bounded a) => Description -> Source -> Parser a
requiredEnum desc srcs = do
  ma <- fieldMaybeEnum Nothing desc srcs
  handleRequired desc ma

{-| A 'Parser' constructor for optional values.
-}
optionalEnum :: (Var a, Enum a, Bounded a) => Description -> Source -> a -> Parser a
optionalEnum desc srcs df = do
  ma <- fieldMaybeEnum (Just df) desc srcs
  maybe (pure df) pure ma

handleRequired :: Description -> Maybe a -> Parser a
handleRequired _ (Just a) = pure a
handleRequired desc Nothing =
  Parser $ \isHelp _ action ->
    if isHelp
      then action isHelp $ error "unreachable"
      else error $
           "No required configuration for \"" <> unDescription desc <> "\"\n" <>
           "Try with '--help' option for more information."

{-| A `Parser` constructor from @cont@.
-}
liftWith :: ((a -> IO ()) -> IO ()) -> Parser a
liftWith cont = Parser $ \b _ action -> cont (action b)

fieldMaybe :: forall a. (Var a) => Maybe a -> Description -> Source -> Parser (Maybe a)
fieldMaybe mdef desc source =
  Parser $ \isHelp conf action -> do
    when isHelp $
      sayString $ helpLine mdef (confParserMods conf) desc source
    action isHelp $ fieldMaybeVal isHelp conf desc source


fieldMaybeVal ::
     forall a. (Var a)
  => Bool
  -> Config
  -> Description
  -> Source
  -> Maybe a
fieldMaybeVal isHelp conf desc (Source srcs) = do
  val <- findValInSrc conf srcs
  let v =
        case (show (typeRep (Proxy :: Proxy a)), val) of
          ("Bool", "") -> "True"
          ("Bool", "true") -> "True"
          ("Bool", "false") -> "False"
          _ -> val
  case fromVar v of
    Nothing ->
      if isHelp
        then Nothing
        else error $
             "Invalid type of value for \"" <> unDescription desc <> "\".\n" <>
             "Try with '--help' option for more information."
    a -> a

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

fieldMaybeEnum :: (Var a, Enum a, Bounded a) => Maybe a -> Description -> Source -> Parser (Maybe a)
fieldMaybeEnum mdef desc source =
  Parser $ \isHelp conf action -> do
    when isHelp $
      sayString $ helpLineEnum mdef (confParserMods conf) desc source
    action isHelp $ fieldMaybeVal isHelp conf desc source

helpLineEnum :: forall a. (Var a, Enum a, Bounded a) => Maybe a -> ParserMods -> Description -> Source -> String
helpLineEnum mdef mods (Description desc) (Source srcs) =
  unlines $
    desc : map (indent 4)
      (helpDefault mdef : helpType (Proxy :: Proxy a) <> helpEnum (Proxy :: Proxy a) : map (helpSource mods) srcs)

helpEnum :: forall a. (Var a, Enum a, Bounded a) => Proxy a -> String
helpEnum _ = if (length enums <= 8)
  then " (" <> (List.intercalate "|" . map toVar) enums <> ")"
  else ""
  where
    enums = [(minBound :: a)..maxBound]

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
