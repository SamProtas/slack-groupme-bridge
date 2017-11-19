{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module GroupMe.Server
    ( runServer
    ) where

import Data.Monoid
import Data.Proxy

import Data.Aeson
import Control.Exception (handle)
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Servant.API
import Servant.Server
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

import Configuration
import GroupMe.Types
import Slack.Types
import Slack.Utilities
import Types
import Utilities


port = 5000


type GroupMeAPI = "group-me" :> ReqBody '[JSON] GroupMeWebhook :> PostNoContent '[JSON] NoContent

api :: Proxy GroupMeAPI
api = Proxy

app :: Config -> Application
app config = serve api (server config)

runServer :: AppContext ()
runServer = do
  logInfoN ("Serving on port: " <> T.pack (show port))
  config <- askConfig
  liftIO $ run port (logStdout $ app config)

serverT :: ServerT GroupMeAPI AppContext
serverT = handleWebhook

server :: Config -> Server GroupMeAPI
server config = enter transformer serverT
  where transformer :: AppContext :~> Handler
        transformer = NT transformer'

        transformer' :: AppContext a -> Handler a
        transformer' r = liftIO (runApp r config)

handleWebhook :: GroupMeWebhook -> AppContext NoContent
handleWebhook webhook = do
  slackConfig <- askSlackConfig
  gmConfig <- askGroupMeConfig
  let mResponse = buildResponse (slackConfig ^. slackChannelId) (gmConfig ^. configGroupMeBotId) webhook
  mapM_ sendMessage mResponse
  return NoContent

buildResponse :: Text -> Text -> GroupMeWebhook -> Maybe SlackBotMessage
buildResponse channelId botId webhook = do
  guard $ webhook ^. gmw_sender_id /= botId
  guard $ not $ T.null finalText
  return SlackBotMessage
            { _sbm_channel = channelId
            , _sbm_text = finalText
            , _sbm_unfurl_links = True
            , _sbm_unfurl_media = True
            , _sbm_username = webhook ^. gmw_name
            , _sbm_link_names = False
            , _sbm_icon_url = icon_url }
    where
      icon_url = if T.null (webhook ^. gmw_avatar_url) then Nothing else Just $ webhook ^. gmw_avatar_url
      attachmentText = T.intercalate "\n" $ webhook ^.. gmw_attachments . traverse . _GroupMeWebhookPicture
      webhookText = webhook ^. gmw_text
      finalText
        | T.null webhookText = attachmentText
        | T.null attachmentText = webhookText
        | otherwise = webhookText <> "\n" <> attachmentText
