{-# LANGUAGE OverloadedStrings #-}
module Slack.Utilities where

import Data.Maybe
import Data.Monoid
import GHC.Generics

import Control.Lens
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wreq
import Network.URI

import Configuration
import GroupMe.Types
import Slack.Types
import Types


baseUrl = "https://slack.com/api/"

postMessageUrl = baseUrl <> "chat.postMessage"

sendMessage :: (MonadReader r m, HasSlackConfig r, MonadIO m) => SlackBotMessage -> m ()
sendMessage message = do
  config <- askSlackConfig
  let opts = defaults & header "Accept" .~ ["application/json"]
                      & header "Content-type" .~ ["application/json; charset=utf-8"]
                      & header "Authorization" .~ ["Bearer " <> config ^. slackAccessKey]
  res <- liftIO $ postWith opts postMessageUrl $ encode message
  return ()

getUserInfoUrl = baseUrl <> "users.info"

getUser :: SlackConfig -> Text -> IO SlackUserResp
getUser config userId = do
  let url = getUserInfoUrl <> "?token=" <> C.unpack (config ^. slackAccessKey) <> "&user=" <> T.unpack userId
  res <- get url
  let body = res ^. responseBody
  return $ fromMaybe (error "Failed to decode") (decode' body)

getUserName :: SlackConfig -> Text -> IO Text
getUserName config userId = (^. sur_user . su_name) <$> getUser config userId
