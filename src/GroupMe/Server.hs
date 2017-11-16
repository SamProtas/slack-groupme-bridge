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


port = 5000


type GroupMeAPI = "group-me" :> ReqBody '[JSON] GroupMeWebhook :> PostNoContent '[JSON] NoContent

api :: Proxy GroupMeAPI
api = Proxy

app :: Config -> Application
app config = serve api (server config)

runServer :: Config -> IO ()
runServer config = putStrLn ("Serving on port: " <> show port) >> run port (logStdout $ app config)

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
  config <- askConfig
  slackConfig <- askSlackConfig
  let mResponse = buildResponse (slackConfig ^. slackChannelId) webhook
  mapM_ sendMessage mResponse
  return NoContent

buildResponse :: Text -> GroupMeWebhook -> Maybe SlackBotMessage
buildResponse channelId webhook = do
  guard $ webhook ^. gmw_sender_type == "user"
  guard $ not $ T.null finalText
  return $ SlackBotMessage channelId finalText True True False $ webhook ^. gmw_name
    where
      attachmentText = T.intercalate "\n" $ webhook ^.. gmw_attachments . traverse . _GroupMeWebhookPicture
      webhookText = webhook ^. gmw_text
      finalText
        | T.null webhookText = attachmentText
        | T.null attachmentText = webhookText
        | otherwise = webhookText <> "\n" <> attachmentText
