{-# LANGUAGE OverloadedStrings #-}
module Slack.Utilities where


import Data.Monoid
import GHC.Generics

import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import Data.Text (Text)
import Network.Wreq

import Configuration
import GroupMe.Types
import Slack.Types


baseUrl = "https://slack.com/api/"

postMessageUrl = baseUrl <> "chat.postMessage"


sendMessage :: SlackConfig -> SlackBotMessage -> IO ()
sendMessage config message = do
  let opts = defaults & header "Accept" .~ ["application/json"]
                      & header "Content-type" .~ ["application/json; charset=utf-8"]
                      & header "Authorization" .~ ["Bearer " <> config ^. slackAccessKey]
  res <- postWith opts postMessageUrl $ encode message
  return ()


webhookToSlackMessage :: Text -> GroupMeWebhook -> SlackBotMessage
webhookToSlackMessage channelId webhook =
  SlackBotMessage channelId (webhook ^. gmw_text) True True False (webhook ^. gmw_name)