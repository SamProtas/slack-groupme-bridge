{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module GroupMe.Types where

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types
import Control.Lens
import Data.Text (Text)

data GroupMeWebhook = GroupMeWebhook
  { _gmw_name :: Text
  , _gmw_sender_type :: Text
  , _gmw_sender_id :: Text
  , _gmw_text :: Text
  , _gmw_avatar_url :: Maybe Text
  , _gmw_attachments :: [GroupMeWebhookAttachment]}
  deriving (Show, Generic)
instance FromJSON GroupMeWebhook where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 5 }

data GroupMeWebhookAttachment = GroupMeWebhookPicture Text
                              | GroupMeWebhookOtherAttachment
                              deriving (Show)
instance FromJSON GroupMeWebhookAttachment where
  parseJSON = withObject "GroupMeWebhookAttachment" $ \v -> do
    type_ <- v .: "type" :: Parser Text
    case type_ of "image" -> GroupMeWebhookPicture <$> v .: "url"
                  _ -> return GroupMeWebhookOtherAttachment


data GroupMeUploadResp = GroupMeUploadResp
  { _gmur_url :: Text
  , _gmur_picture_url :: Text }
  deriving (Show, Generic)
instance FromJSON GroupMeUploadResp where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 6 }

newtype GroupMeUploadRespWrapper = GroupMeUploadRespWrapper
  { _gmurw_payload :: GroupMeUploadResp }
  deriving (Show, Generic)
instance FromJSON GroupMeUploadRespWrapper where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 7}



data GroupMeBotMessage = GroupMeBotMessage
  { _gmb_bot_id :: Text
  , _gmb_text :: Text
  , _gmb_token :: Text
  , _gmb_picture_url :: Maybe Text }
  deriving (Show, Eq, Generic)
instance ToJSON GroupMeBotMessage where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 5 }



makePrisms ''GroupMeWebhookAttachment
makeLenses 'GroupMeWebhook
makeLenses 'GroupMeBotMessage
makeLenses 'GroupMeUploadResp
makeLenses 'GroupMeUploadRespWrapper
