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
  , _sbm_icon_url :: Maybe Text
  }
  deriving (Show, Generic)
instance ToJSON SlackBotMessage where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 5
                                          , omitNothingFields = True }


data SlackRtmConnectResp = SlackRtmConnectResp
  { _srtmr_ok :: Bool
  , _srtmr_url :: String }
  deriving (Show, Generic)
instance FromJSON SlackRtmConnectResp where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 7 }


data SlackEvent = SlackEventMessage SlackMessage
                | SlackEventFileShare SlackFileShare
                | SlackEventOther Value
                deriving (Show, Generic)
instance FromJSON SlackEvent where
  parseJSON = withObject "SlackEvent" $ \v -> do
    type_ <- v .: "type" :: Parser Text
    mSubtype <- v .:? "subtype" :: Parser (Maybe Text)
    case (type_, mSubtype) of ("message", Nothing) -> SlackEventMessage <$> parseJSON (Object v)
                              ("message", Just "file_share") -> SlackEventFileShare <$> parseJSON (Object v)
                              _ -> return $ SlackEventOther (Object v)

data SlackMessage = SlackMessage
  { _sm_channel :: Text
  , _sm_text :: Text
  , _sm_user :: Text }
  deriving (Show, Generic)
instance FromJSON SlackMessage where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 4 }

data SlackFileShare = SlackFileShare
  { _sfs_user :: Text
  , _sfs_channel :: Text
  , _sfs_file :: SlackFile }
  deriving (Show, Generic)
instance FromJSON SlackFileShare where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 5 }

data SlackFile = SlackFile
  { _sf_name :: Text
  , _sf_url_private :: Text
  , _sf_initial_comment :: Maybe SlackFileComment }
  deriving (Show, Generic)
instance FromJSON SlackFile where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 4 }

data SlackFileComment = SlackFileComment
  { _sfc_comment :: Text
  , _sfc_user :: Text }
  deriving (Show, Generic)
instance FromJSON SlackFileComment where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 5 }


data SlackUserResp = SlackUserResp
  { _sur_ok :: Bool
  , _sur_user :: SlackUser }
  deriving (Show, Generic)
instance FromJSON SlackUserResp where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 5 }

data SlackUser = SlackUser
  { _su_id :: Text
  , _su_name :: Text }
  deriving (Show, Generic)
instance FromJSON SlackUser where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 4}


makeLenses 'SlackBotMessage
makeLenses 'SlackRtmConnectResp
makePrisms ''SlackEvent
makeLenses 'SlackMessage
makeLenses 'SlackUserResp
makeLenses 'SlackUser
makeLenses 'SlackFileShare
makeLenses 'SlackFile
makeLenses 'SlackFileComment