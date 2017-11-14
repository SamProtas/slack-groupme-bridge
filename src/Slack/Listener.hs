{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Slack.Listener where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TBQueue
import Data.HashMap.Strict (HashMap)
import Data.Time
import Data.Time.Clock.POSIX
import Network.Wreq
import Caching.ExpiringCacheMap.HashECM
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Control.Concurrent.Async
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

tshow :: Show a => a -> Text
tshow = T.pack . show

startWsListener :: Config -> IO ()
startWsListener config = do
  res <- rtmConnect $ config ^. configSlack
  rtmListen config res


rtmConnect :: SlackConfig -> IO SlackRtmConnectResp
rtmConnect config = do
  putStrLn "Obtaining a websocket url."
  res <- post "https://slack.com/api/rtm.connect" ["token" := (config ^. slackAccessKey)]
  let eitherBody = eitherDecode' $ res ^. responseBody
  case eitherBody of Left err -> error err  -- TODO: handle error better
                     Right body -> return body


rtmListen :: Config -> SlackRtmConnectResp -> IO ()
rtmListen config rtmConnectResp = do
  let uri = fromMaybe (error "Could not parse websockets url.") $ parseAbsoluteURI $ rtmConnectResp ^. srtmr_url
      authority' = fromMaybe (error "Could not parse webhsockets domain.") $ uriAuthority uri
      domain = uriRegName authority'
      path' = uriPath uri
  nameCache <- newCache $ config ^. configSlack
  runSecureClient domain 443 path' $ \connection -> do
    putStrLn "Listening to the slack websocket."
    queue <- newTBQueueIO 100
    let readLoop = do
          raw <- receiveData connection
          let eitherEvent = eitherDecodeStrict' raw :: Either String SlackEvent
          case eitherEvent of Left err -> print err
                              Right event -> atomically (writeTBQueue queue event)
          readLoop
    let writeLoop = atomically (readTBQueue queue) >>= handleEvent config nameCache >> writeLoop
    race_ readLoop writeLoop


handleEvent :: Config -> NameCache s -> SlackEvent -> IO ()
handleEvent config cache (SlackEventOther _) = return ()
handleEvent config cache event =
  forM_ (buildResponse config event) $ \(userId, msgBuilder) -> do
                                          userName <- lookupECM cache userId
                                          sendMessage config $ msgBuilder userName

buildResponse :: Config -> SlackEvent -> Maybe (Text, Text -> GroupMeBotMessage)
buildResponse _ (SlackEventOther _) = mzero
buildResponse config (SlackEventMessage message) = do
  let msgBuilder userName = GroupMeBotMessage
                              (config ^. configGroupMe . configGroupMeBotId)
                              (message ^. sm_text <> "\n\n      - " <> userName)
                              (config ^. configGroupMe . configGroupMeAccessKey)

  return (message ^. sm_user, msgBuilder)

type NameCache s = (ECM IO MVar s HashMap Text Text)

newCache :: SlackConfig -> IO (NameCache s)
newCache config = do
  let cacheAccess = consistentDuration 100 $ \state userId -> do
                                                userName <- getUserName config userId
                                                return (state, userName)
      cacheTick = do
        time <- getPOSIXTime
        return $ round (time * 100)
      cacheFreq = 1
      cacheLRU = CacheWithLRUList 100 100 200
  newECMIO cacheAccess cacheTick cacheFreq cacheLRU
