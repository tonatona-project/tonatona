{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tonatona.Sample where

import Control.Monad.Reader (ReaderT)
import Data.Aeson (ToJSON(toJSON))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Void (Void, absurd)
import Database.Persist.Sql (Migration, SqlBackend, (==.), entityVal, insert_, selectList)
import Database.Persist.TH (mkMigrate, mkPersist, mpsGenerateLenses, persistLowerCase, share, sqlSettings)
import System.Envy (FromEnv(..), Var(..), (.!=), envMaybe)
import Servant
import Tonatona (Plug(..), TonaM, askConf)
import qualified Tonatona as Tona
import qualified Tonatona.Db.Postgresql as TonaDbPostgres
import qualified Tonatona.Db.Sqlite as TonaDbSqlite
import Tonatona.Email.Sendmail (Address(..), TonaEmailShared(..), simpleMail')
import qualified Tonatona.Email.Sendmail as TonaEmail
import qualified Tonatona.Environment as TonaEnv
import Tonatona.Logger (logDebug, logInfo, stdoutLogger)
import qualified Tonatona.Logger as TonaLogger
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
  "send-email" :> Get '[JSON] Int

server :: ServerT API (TonaM Config Shared)
server = getFoo :<|> tagServer :<|> redirectExample :<|> sendEmailExample

getFoo :: TonaM Config Shared Int
getFoo = do
  $(logInfo) "in getFoo, returning 1"
  pure 1

tagServer :: ServerT TagAPI (TonaM Config Shared)
tagServer = postTag :<|> getTag

postTag :: Text -> Text -> TonaM Config Shared ()
postTag name val = do
  sharedDbRun $ do
    $(logInfo) $
      "in postTag, in TonaDb.run, inserting a tag with name = " <>
      name <> ", val = " <> val
    insert_ (Tag name val)

getTag :: Text -> TonaM Config Shared [Text]
getTag name = do
  tagEnts <- sharedDbRun $
    selectList [TagName ==. name] []
  pure $ tagValue . entityVal <$> tagEnts

redirectExample :: TonaM Config Shared Void
redirectExample = TonaServant.redirect "https://google.com"

sendEmailExample :: TonaM Config Shared Int
sendEmailExample = do
  let mail =
        simpleMail'
          (Address Nothing "foo@example.com")
          (Address Nothing "bar@example.com")
          "test email subject from foo to bar"
          "This is a test email from foo@example.com to bar@example.com."
  TonaEmail.send mail
  pure 0

instance ToJSON Void where toJSON = absurd

app :: IO ()
app =
  Tona.run $ do
    $(logDebug) "About to run migration..."
    sharedDbMigrate migrateAll
    $(logDebug) "About to run web server..."
    TonaServant.run @API server

-- Config

data DbToUse = PostgreSQL | Sqlite deriving Show

instance FromEnv DbToUse where
  fromEnv = envMaybe "DB_TO_USE" .!= PostgreSQL

instance Var DbToUse where
  toVar PostgreSQL = "postgresql"
  toVar Sqlite = "sqlite"

  fromVar "postgresql" = Just PostgreSQL
  fromVar "sqlite" = Just Sqlite
  fromVar _ = Nothing

data Config = Config
  { tonaDbPostgres :: TonaDbPostgres.Config
  , tonaDbSqlite :: TonaDbSqlite.Config
  , dbToUse :: DbToUse
  , tonaEnv :: TonaEnv.Config
  , tonaServant :: TonaServant.Config
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv =
    Config
      <$> fromEnv
      <*> fromEnv
      <*> fromEnv
      <*> fromEnv
      <*> fromEnv

instance TonaDbPostgres.HasConfig Config where
  config = tonaDbPostgres

instance TonaDbSqlite.HasConfig Config where
  config = tonaDbSqlite

instance TonaEnv.HasConfig Config where
  config = tonaEnv

instance TonaServant.HasConfig Config where
  config = tonaServant


-- Shared

data Shared = Shared
  { tonaDb :: SharedDb
  , tonaEmail :: TonaEmail.Shared
  , tonaLogger :: TonaLogger.Shared
  }


data SharedDb
  = SharedSqlite TonaDbSqlite.Shared
  | SharedPostgres TonaDbPostgres.Shared

sharedDbRun ::
     (TonaDbPostgres.HasShared shared, TonaDbSqlite.HasShared shared)
  => ReaderT SqlBackend (TonaM Config shared) a
  -> TonaM Config shared a
sharedDbRun query = do
  conf <- askConf
  case dbToUse conf of
    Sqlite -> TonaDbSqlite.run query
    PostgreSQL -> TonaDbPostgres.run query

sharedDbMigrate ::
     (TonaDbPostgres.HasShared shared, TonaDbSqlite.HasShared shared)
  => Migration
  -> TonaM Config shared ()
sharedDbMigrate migration = do
  conf <- askConf
  case dbToUse conf of
    Sqlite -> TonaDbSqlite.runMigrate migration
    PostgreSQL -> TonaDbPostgres.runMigrate migration

instance Plug Config Shared where
  init conf = do
    let db =
          case dbToUse conf of
            Sqlite -> SharedSqlite <$> TonaDbSqlite.init conf stdoutLogger
            PostgreSQL -> SharedPostgres <$> TonaDbPostgres.init conf stdoutLogger
    Shared
      <$> db
      <*> TonaEmail.init
      <*> TonaLogger.init stdoutLogger

instance TonaDbSqlite.HasShared Shared where
  shared s =
    case (tonaDb :: Shared -> SharedDb) s of
      SharedSqlite sqliteShared -> sqliteShared
      _ -> error "This is not an sqlite shared type."

instance TonaDbPostgres.HasShared Shared where
  shared s =
    case (tonaDb :: Shared -> SharedDb) s of
      SharedPostgres postgresShared -> postgresShared
      _ -> error "This is not a postgres shared type."

instance TonaLogger.HasShared Shared where
  shared = tonaLogger

instance TonaEmailShared Shared where
  shared = tonaEmail
