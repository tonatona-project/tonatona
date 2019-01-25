{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TonaApp.Main where

import Tonalude

import Database.Persist.Sql ((==.), entityVal, insert_, selectList)
import Network.Mail.Mime (Address(..), renderSendMail, simpleMail')
import Servant

import TonaApp.Db
import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.Servant as TonaServant
import qualified Tonatona.Persist.Sqlite as TonaDb



-- App


app :: RIO Config ()
app = do
  -- Tonatona.Logger plugin enables to use logger functions without any configurations.
  TonaLogger.logDebug $ display ("About to run migration..." :: Text)
  TonaDb.runMigrate migrateAll
  -- Configurations are accessable by 'asks' as follows.
  port <- asks (TonaServant.port . config)
  TonaLogger.logDebug $
    ("About to run web server on port " <> display port <> " ...")
  TonaServant.run @API server



-- Servant API defs


type TagAPI = "tag" :>
  ( Capture "tagname" Text :> Capture "tagvalue" Text :> Post '[JSON] () :<|>
    Capture "tagname" Text :> Get '[JSON] [Text]
  )

type API =
  "logger-example" :> Get '[JSON] Int :<|>
  TagAPI :<|>
  "redirect-example" :> Get '[JSON] Void :<|>
  "send-email" :> Get '[JSON] Int :<|>
  "error-example" :> Get '[JSON] Int

server :: ServerT API (RIO Config)
server =
  loggerExample :<|>
  tagServer :<|>
  redirectExample :<|>
  sendEmailExample :<|>
  errorExample


-- Handlers



-- As you can see the type, any plugins can be used in @TonaServer.run@.
loggerExample :: RIO Config Int
loggerExample = do
  TonaLogger.logInfo $ display ("in loggerExample, returning 1" :: Text)
  pure 1

tagServer :: ServerT TagAPI (RIO Config)
tagServer = postTag :<|> getTag

postTag :: Text -> Text -> RIO Config ()
postTag name val = do
  TonaDb.run $ do
    -- By using 'lift', any plugins are available in @Tonatona.Db.*.run@
    lift $
      TonaLogger.logInfo $ display $
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
  throwM $ err404
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
