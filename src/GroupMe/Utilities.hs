{-# LANGUAGE OverloadedStrings #-}
module GroupMe.Utilities where


import Control.Monad
import Data.Monoid

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Text as T
import Network.Wreq

import Configuration
import GroupMe.Types



baseUrl = "https://api.groupme.com/v3"
botMessageUrl = baseUrl <> "/bots/post"

sendMessage :: MonadIO m => GroupMeBotMessage -> m ()
sendMessage message = void $ liftIO $ postWith opts botMessageUrl $ encode message
  where opts = defaults & header "Accept" .~ ["application/json"]
                        & header "Content-type" .~ ["application/json; charset=utf-8"]