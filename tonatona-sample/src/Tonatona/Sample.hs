{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tonatona.Sample where

import RIO

#if MIN_VERSION_aeson(1,3,0)
import Data.Void (Void)
#else
import Data.Void (Void, absurd)
import Data.Aeson (ToJSON(toJSON))
#endif
import Database.Persist.Sql ((==.), entityVal, insert_, selectList)
import Database.Persist.TH (mkMigrate, mkPersist, mpsGenerateLenses, persistLowerCase, share, sqlSettings)
import Network.Mail.Mime (Address(..), renderSendMail, simpleMail')
import Servant
import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona as Tona
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.Servant as TonaServant
import qualified Tonatona.Persist.Sqlite as TonaDb


-- DB entity defs


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



-- App


app :: IO ()
app =
  Tona.run $ do
    -- Logger functions are available only by adding Tonatona.Logger instance in application @Config@ type.
    logDebug $ display ("About to run migration..." :: Text)
    TonaDb.runMigrate migrateAll
    -- Configurations are accessable by 'asks' as follows.
    port <- asks (TonaServant.port . config)
    logDebug $
      ("About to run web server on port " <> display port <> " ...")
    TonaServant.run @API server



-- Servant API defs


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

server :: ServerT API (RIO Config)
server = getFoo :<|> tagServer :<|> redirectExample :<|> sendEmailExample :<|> errorExample

-- As you can see the type, any plugins can be used in @TonaServer.run@.
getFoo :: RIO Config Int
getFoo = do
  logInfo $ display ("in getFoo, returning 1" :: Text)
  pure 1

tagServer :: ServerT TagAPI (RIO Config)
tagServer = postTag :<|> getTag

postTag :: Text -> Text -> RIO Config ()
postTag name val = do
  TonaDb.run $ do
    -- By using 'lift', any plugins are available in @Tonatona.Db.*.run@
    lift $
      logInfo $ display $
        "in postTag, in TonaDb.run, inserting a tag with name = " <>
        name <> ", val = " <> val
    insert_ (Tag name val)

getTag :: Text -> RIO Config [Text]
getTag name = do
  tagEnts <- TonaDb.run $
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
  liftIO $ renderSendMail mail
  pure 0

errorExample :: RIO Config Int
errorExample = do
  throwIO $ err404

#if MIN_VERSION_aeson(1,3,0)
#else
instance ToJSON Void where toJSON = absurd
#endif



-- Config


data Config = Config
  { tonaLogger :: TonaLogger.Config
  , tonaDb :: TonaDb.Config
  , tonaServant :: TonaServant.Config
  }

instance HasConfig Config TonaLogger.Config where
  config = tonaLogger

instance HasConfig Config TonaDb.Config where
  config = tonaDb

instance HasConfig Config TonaServant.Config where
  config = tonaServant

instance HasParser Config where
  parser = Config
      <$> parser
      <*> parser
      <*> parser
