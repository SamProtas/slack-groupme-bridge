{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Slack.Listener where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TBQueue
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.HashMap.Strict (HashMap)
import Data.Time
import Data.Time.Clock.POSIX
import Network.Wreq
import Caching.ExpiringCacheMap.HashECM
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Control.Concurrent.Async.Lifted.Safe
import Data.Text (Text)
import qualified Data.Text as T
import Wuss hiding (Config)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
import Network.URI

import Configuration
import Slack.Types
import Slack.Utilities hiding (sendMessage)
import GroupMe.Types
import GroupMe.Utilities
import Types

tshow :: Show a => a -> Text
tshow = T.pack . show

startWsListener :: (MonadReader r m, HasConfig r, MonadIO m, MonadBaseControl IO m, Forall (Pure m)) => m ()
startWsListener = do
  res <- rtmConnect
  rtmListen res


rtmConnect :: (MonadReader r m, HasSlackConfig r, MonadIO m) => m SlackRtmConnectResp
rtmConnect = do
  config <- askSlackConfig
  liftIO $ putStrLn "Obtaining a websocket url."
  res <- liftIO $ post "https://slack.com/api/rtm.connect" ["token" := (config ^. slackAccessKey)]
  let eitherBody = eitherDecode' $ res ^. responseBody
  case eitherBody of Left err -> error err  -- TODO: handle error better
                     Right body -> return body


rtmListen :: (MonadReader r m, HasConfig r, MonadIO m, MonadBaseControl IO m, Forall (Pure m)) =>
             SlackRtmConnectResp ->
             m ()
rtmListen rtmConnectResp = do
  let uri = fromMaybe (error "Could not parse websockets url.") $ parseAbsoluteURI $ rtmConnectResp ^. srtmr_url
      authority' = fromMaybe (error "Could not parse webhsockets domain.") $ uriAuthority uri
      domain = uriRegName authority'
      path' = uriPath uri
  nameCache <- newCache
  liftBaseOp (runSecureClient domain 443 path') $ \connection -> do
    liftIO $ putStrLn "Listening to the slack websocket."
    queue <- liftIO $ newTBQueueIO 100
    let readLoop = do
          raw <- liftIO $ receiveData connection
          let eitherEvent = eitherDecodeStrict' raw :: Either String SlackEvent
          case eitherEvent of Left err -> liftIO $ print err
                              Right event -> liftIO $ atomically (writeTBQueue queue event)
          readLoop
    let writeLoop = do
          event <- liftIO (atomically $ readTBQueue queue)
          handleEvent nameCache event
          writeLoop
    race_ readLoop writeLoop


handleEvent :: (MonadReader r m, HasConfig r, MonadIO m) => NameCache -> SlackEvent -> m ()
handleEvent cache (SlackEventOther _) = return ()
handleEvent cache event = do
  gmConfig <- askGroupMeConfig
  liftIO $ forM_ (buildResponse gmConfig event) $ \(userId, msgBuilder) -> do
                                                      userName <- lookupECM cache userId
                                                      sendMessage $ msgBuilder userName

buildResponse :: GroupMeConfig -> SlackEvent -> Maybe (Text, Text -> GroupMeBotMessage)
buildResponse _ (SlackEventOther _) = mzero
buildResponse config (SlackEventMessage message) = do
  let msgBuilder userName = GroupMeBotMessage
                              (config ^. configGroupMeBotId)
                              (message ^. sm_text <> "\n\n      - " <> userName)
                              (config ^. configGroupMeAccessKey)

  return (message ^. sm_user, msgBuilder)

type NameCache = ECM IO MVar () HashMap Text Text

newCache :: (MonadReader r m, HasSlackConfig r, MonadIO m) => m NameCache
newCache = do
  config <- askSlackConfig
  let cacheAccess = consistentDuration 100 $ \state userId -> do
                                                userName <- getUserName config userId
                                                return (state, userName)
      cacheTick = do
        time <- getPOSIXTime
        return $ round (time * 100)
      cacheFreq = 1
      cacheLRU = CacheWithLRUList 100 100 200
  liftIO $ newECMIO cacheAccess cacheTick cacheFreq cacheLRU
