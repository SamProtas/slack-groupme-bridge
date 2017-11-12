{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Slack.Types where


import Data.Monoid
import GHC.Generics

import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import Data.Text (Text)
import Network.Wreq

import Configuration


data SlackBotMessage = SlackBotMessage
  { _sbm_channel :: Text
  , _sbm_text :: Text
  , _sbm_unfurl_links :: Bool
  , _sbm_unfurl_media :: Bool
  , _sbm_link_names :: Bool
  , _sbm_username :: Text
  }
  deriving (Show, Generic)
instance ToJSON SlackBotMessage where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 5
                                          , omitNothingFields = True }


makeLenses 'SlackBotMessage