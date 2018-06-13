{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tonatona.Sample
  where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Void
import Database.Persist.Postgresql
import Database.Persist.TH
import System.Envy (FromEnv(..))
import Servant
import Tonatona (Plug(..), TonaM)
import qualified Tonatona as Tona
import qualified Tonatona.IO as TonaIO
import Tonatona.Db (TonaDbConfig(..), TonaDbShared(..))
import qualified Tonatona.Db as TonaDb
import Tonatona.Environment (TonaEnvConfig(..))
import qualified Tonatona.Environment as TonaEnv
import qualified Tonatona.Servant as TonaServant
import Tonatona.Servant (TonaServantConfig(..))


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
  "redirect-example" :> Get '[JSON] Void

server :: ServerT API (TonaM Config Shared)
server = getFoo :<|> tagServer :<|> redirectExample

getFoo :: TonaM Config Shared Int
getFoo = pure 1

tagServer :: ServerT TagAPI (TonaM Config Shared)
tagServer = postTag :<|> getTag

postTag :: Text -> Text -> TonaM Config Shared ()
postTag name val = do
  TonaDb.run $
    insert_ (Tag name val)

getTag :: Text -> TonaM Config Shared [Text]
getTag name = do
  tagEnts <- TonaDb.run $
    selectList [TagName ==. name] []
  pure $ tagValue . entityVal <$> tagEnts

redirectExample :: TonaM Config Shared Void
redirectExample = TonaServant.redirect "https://google.com"

instance ToJSON Void where toJSON = absurd

app :: IO ()
app =
  Tona.run $ do
    liftIO $ putStrLn "About to run migration..."
    TonaDb.runMigrate migrateAll
    liftIO $ putStrLn "About to run web server..."
    TonaServant.run @Config @Shared @API server
  -- TonaDb.run $
  --   TonaDb.migrate
  -- TonaIO.run $ \_conf shared' ->
  --   putStrLn $ "dbPool (" <> TonaDb.dbPool (TonaDb.shared shared') <> ") is shared"
  -- TonaDb.run $
  --   TonaDb.migrate


-- Config

data Config = Config
  { tonaDb :: TonaDb.Config
  , tonaEnv :: TonaEnv.Config
  , tonaServant :: TonaServant.Config
  }
  deriving (Show)

instance FromEnv Config where
  fromEnv = Config
    <$> fromEnv
    <*> fromEnv
    <*> fromEnv

instance TonaDbConfig Config where
  config = tonaDb

instance TonaEnvConfig Config where
  config = tonaEnv

instance TonaServantConfig Config where
  config = tonaServant


-- Shared


data Shared = Shared
  { tonaDb :: TonaDb.Shared
  }
  deriving (Show)

instance Plug Config Shared where
  init conf = Shared
    <$> TonaDb.init conf

instance TonaDbShared Shared where
  shared = tonaDb
