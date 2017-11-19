{-# LANGUAGE OverloadedStrings #-}
module GroupMe.Utilities where


import Control.Monad
import Data.Monoid

import Control.Exception.Safe
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Mime
import Network.Wreq

import Configuration
import GroupMe.Types
import Types


baseUrl = "https://api.groupme.com/v3"
botMessageUrl = baseUrl <> "/bots/post"

sendMessage :: MonadIO m => GroupMeBotMessage -> m ()
sendMessage message = void $ liftIO $ postWith opts botMessageUrl $ encode message
  where opts = defaults & header "Accept" .~ ["application/json"]
                        & header "Content-type" .~ ["application/json; charset=utf-8"]

basePictureUrl = "https://image.groupme.com"
uploadPictureUrl = basePictureUrl <> "/pictures"

uploadPicture :: (MonadReader r m, HasGroupMeConfig r, MonadIO m, MonadThrow m) => Text -> ByteString -> m GroupMeUploadResp
uploadPicture pictureName pictureData = do
  config <- askGroupMeConfig
  let contentType = defaultMimeLookup pictureName
      url = T.unpack $ uploadPictureUrl <> "?token=" <> config ^. configGroupMeAccessKey
      opts = defaults & header "Content-Type" .~ [contentType]
  res <- liftIO $ postWith opts url pictureData
  let body = res ^. responseBody
  case eitherDecode body of Left err -> throwString err
                            Right (GroupMeUploadRespWrapper uploadResponse) -> return uploadResponse
