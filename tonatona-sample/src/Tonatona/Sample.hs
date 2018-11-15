{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tonatona.Sample where

import RIO

import Data.Aeson (ToJSON(toJSON))
import Data.Void (Void, absurd)
import Database.Persist.Sql (Migration, SqlBackend, (==.), entityVal, insert_, selectList)
import Database.Persist.TH (mkMigrate, mkPersist, mpsGenerateLenses, persistLowerCase, share, sqlSettings)
import Servant
import TonaParser (Parser(..), Var(..), (.||), argLong, envVar, optionalVal)
import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona as Tona
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.Db.Postgresql as TonaDbPostgres
import qualified Tonatona.Db.Sqlite as TonaDbSqlite
import Tonatona.Email.Sendmail (Address(..), simpleMail')
import qualified Tonatona.Email.Sendmail as TonaEmail
import qualified Tonatona.Servant as TonaServant


$(share
  [ mkPersist sqlSettings {mpsGenerateLenses = False}
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
  Tag
    name      Text
    value      Text

    deriving Eq
    deriving Show
    |]
 )

type TagAPI = "tag" :> (
  Capture "tagname" Text :> Capture "tagvalue" Text :> Post '[JSON] () :<|>
    Capture "tagname" Text :> Get '[JSON] [Text]
  )

type API =
  "foo" :> Get '[JSON] Int :<|>
  TagAPI :<|>
  "redirect-example" :> Get '[JSON] Void :<|>
  "send-email" :> Get '[JSON] Int :<|>
  "error-example" :> Get '[JSON] Int

app :: IO ()
app =
  Tona.run $ do
    logDebug $ display ("About to run migration..." :: Text)
    sharedDbMigrate migrateAll
    logDebug $ display ("About to run web server..." :: Text)
    TonaServant.run @API server

server :: ServerT API (RIO Config)
server = getFoo :<|> tagServer :<|> redirectExample :<|> sendEmailExample :<|> errorExample

getFoo :: RIO Config Int
getFoo = do
  logInfo $ display ("in getFoo, returning 1" :: Text)
  pure 1

tagServer :: ServerT TagAPI (RIO Config)
tagServer = postTag :<|> getTag

postTag :: Text -> Text -> RIO Config ()
postTag name val = do
  sharedDbRun $ do
    -- logInfo $ display $
    --   "in postTag, in TonaDb.run, inserting a tag with name = " <>
    --   name <> ", val = " <> val
    insert_ (Tag name val)

getTag :: Text -> RIO Config [Text]
getTag name = do
  tagEnts <- sharedDbRun $
    selectList [TagName ==. name] []
  pure $ tagValue . entityVal <$> tagEnts

redirectExample :: RIO Config Void
redirectExample = TonaServant.redirect "https://google.com"

sendEmailExample :: RIO Config Int
sendEmailExample = do
  let mail =
        simpleMail'
          (Address Nothing "foo@example.com")
          (Address Nothing "bar@example.com")
          "test email subject from foo to bar"
          "This is a test email from foo@example.com to bar@example.com."
  TonaEmail.send mail
  pure 0

errorExample :: RIO Config Int
errorExample = do
  throwIO $ err404

instance ToJSON Void where toJSON = absurd

-- Config

data DbToUse = PostgreSQL | Sqlite deriving Show

instance HasParser a DbToUse where
  parser =
    optionalVal
      "Database type to use (postgresql|sqlite)"
      (argLong "db-to-use" .|| envVar "DB_TO_USE")
      PostgreSQL

instance Var DbToUse where
  toVar PostgreSQL = "postgresql"
  toVar Sqlite = "sqlite"

  fromVar "postgresql" = Just PostgreSQL
  fromVar "sqlite" = Just Sqlite
  fromVar _ = Nothing

data Config = Config
  { tonaLogger :: TonaLogger.Config
  , dbToUse :: DbToUse
  , tonaDb :: TonaDbConfig
  , tonaServant :: TonaServant.Config
  }

data TonaDbConfig
  = TonaDbPostgres TonaDbPostgres.Config
  | TonaDbSqlite TonaDbSqlite.Config

instance HasConfig Config TonaLogger.Config where
  config = tonaLogger

instance HasConfig Config TonaDbConfig where
  config = tonaDb

instance HasConfig Config TonaDbPostgres.Config where
  config c = case tonaDb c of
    TonaDbPostgres a -> a
    _ -> error "This is not an postgres config type."

instance HasConfig Config TonaDbSqlite.Config where
  config c = case tonaDb c of
    TonaDbSqlite a -> a
    _ -> error "This is not an sqlite config type."

instance HasConfig Config DbToUse where
  config = dbToUse

instance HasConfig Config TonaServant.Config where
  config = tonaServant

dbParser :: DbToUse -> Parser a TonaDbConfig
dbParser PostgreSQL = TonaDbPostgres <$> parser
dbParser Sqlite = TonaDbSqlite <$> parser

instance HasParser a Config where
  parser = do
    dbToUse <- parser
    Config
      <$> parser
      <*> (pure dbToUse)
      <*> dbParser dbToUse
      <*> parser



-- DB operators


sharedDbRun ::
     ReaderT SqlBackend (RIO Config) a -> RIO Config a
sharedDbRun query = do
  dbToUse <- asks config
  case dbToUse of
    Sqlite -> TonaDbSqlite.run query
    PostgreSQL -> TonaDbPostgres.run query

sharedDbMigrate :: Migration -> RIO Config ()
sharedDbMigrate migration = do
  dbToUse <- asks config
  case dbToUse of
    Sqlite -> TonaDbSqlite.runMigrate migration
    PostgreSQL -> TonaDbPostgres.runMigrate migration
