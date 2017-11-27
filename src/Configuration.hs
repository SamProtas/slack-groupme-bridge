{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Configuration where

import Control.Monad
import Data.Functor.Compose
import System.Environment
import Text.Read

import Configuration.Dotenv
import Control.Lens
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import System.Log.Raven.Types
import Data.Text (Text)

import Monitoring.Sentry.Configuration


data Config = Config
  { _configGroupMe :: GroupMeConfig
  , _configSlack :: SlackConfig
  , _configSentryService :: SentryService }

data GroupMeConfig = GroupMeConfig
  { _configGroupMeAccessKey :: Text
  , _configGroupMeBotId :: Text }
  deriving (Show, Eq)

data SlackConfig = SlackConfig
  { _slackAccessKey :: B.ByteString
  , _slackChannelId :: Text }
  deriving (Show)

makeLenses 'GroupMeConfig
makeLenses 'SlackConfig
makeLenses 'Config

class (HasSlackConfig c, HasGroupMeConfig c, HasSentryService c) => HasConfig c where
  config :: Lens' c Config

instance HasConfig Config where
  config = id

instance HasSentryService Config where
  sentryService = configSentryService

class HasSlackConfig c where
  slackConfig :: Lens' c SlackConfig
instance HasSlackConfig SlackConfig where
  slackConfig = id
instance HasSlackConfig Config where
  slackConfig = configSlack

class HasGroupMeConfig c where
  groupMeConfig :: Lens' c GroupMeConfig
instance HasGroupMeConfig GroupMeConfig where
  groupMeConfig = id
instance HasGroupMeConfig Config where
  groupMeConfig = configGroupMe

eitherGetConfig :: IO (Either String Config)
eitherGetConfig = do
  loadFile False "./.env"
  getCompose $
     Config <$>
      (GroupMeConfig <$>
        cLookupText "GROUPME_ACCESS_KEY" <*>
        cLookupText "GROUPME_BOT_ID") <*>
      (SlackConfig <$>
        cLookupBs "SLACK_ACCESS_KEY" <*>
        cLookupText "SLACK_CHANNEL_ID") <*>
      cLookupSentry "SENTRY_DSN"
  where
    -- TODO: Handle empty strings as missing
    maybeToEither :: String -> Maybe b -> Either String b
    maybeToEither message = maybe (Left message) Right

    maybeToEitherInt :: String -> Maybe String -> Either String Int
    maybeToEitherInt message mVar = join . fmap readEither $ maybeToEither message mVar

    maybeToEitherBs eitherS = fmap C.pack . maybeToEither eitherS
    maybeToEitherText eitherS = fmap T.pack . maybeToEither eitherS

    lookupText var = maybeToEitherText var <$> lookupEnv var
    lookupBs var = maybeToEitherBs var <$> lookupEnv var
    lookupInt var = maybeToEitherInt var <$> lookupEnv var
    lookupString var = maybeToEither var <$> lookupEnv var

    cLookupText = Compose . lookupText
    cLookupBs = Compose . lookupBs
    cLookupInt = Compose . lookupInt

    cLookupSentry var = Compose $ do
      eitherRes <- lookupString var
      case eitherRes of Right dsn -> fromDsn dsn >>= return . Right
                        Left l -> return $ Left l


    
askConfig :: (MonadReader r m, HasConfig r) => m Config
askConfig = view config <$> ask

askGroupMeConfig :: (MonadReader r m, HasGroupMeConfig r) => m GroupMeConfig
askGroupMeConfig = view groupMeConfig <$> ask

askSlackConfig :: (MonadReader r m, HasSlackConfig r) => m SlackConfig
askSlackConfig = view slackConfig <$> ask

askSentryService :: (MonadReader r m, HasSentryService r) => m SentryService
askSentryService = view sentryService <$> ask
