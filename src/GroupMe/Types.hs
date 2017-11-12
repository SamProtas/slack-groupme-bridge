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
  , _gmw_text :: Text
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
    case type_ of "image" -> v .: "url" >>= return . GroupMeWebhookPicture
                  _ -> return GroupMeWebhookOtherAttachment





data GroupMeBotMessage = GroupMeBotMessage
  { _gmb_bot_id :: Text
  , _gmb_text :: Text }
  deriving (Show, Eq, Generic)
instance ToJSON GroupMeBotMessage where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 5 }



makePrisms ''GroupMeWebhookAttachment
makeLenses ''GroupMeWebhook
makeLenses ''GroupMeBotMessage
