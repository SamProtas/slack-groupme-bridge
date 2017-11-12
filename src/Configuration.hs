{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Configuration where

import Control.Monad
import Data.Functor.Compose
import System.Environment
import Text.Read

import Configuration.Dotenv
import Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T

import Data.Text (Text)


data Config = Config
  { _configGroupMe :: GroupMeConfig
  , _configSlack :: SlackConfig }
  deriving (Show)

data GroupMeConfig = GroupMeConfig
  { _configGroupMeAccessKey :: Text
  , _configGroupMeGroupId :: Int }
  deriving (Show, Eq)

data SlackConfig = SlackConfig
  { _slackAccessKey :: B.ByteString
  , _slackChannelId :: Text}
  deriving (Show)

makeLenses 'GroupMeConfig
makeLenses 'SlackConfig
makeLenses 'Config


eitherGetConfig :: IO (Either String Config)
eitherGetConfig = do
  loadFile False "./.env"
  getCompose $
     Config <$>
      (GroupMeConfig <$>
        cLookupText "GROUPME_ACCESS_KEY" <*>
        cLookupInt "GROUPME_GROUP_ID") <*>
      (SlackConfig <$>
        cLookupBs "SLACK_ACCESS_KEY" <*>
        cLookupText "SLACK_CHANNEL_ID")
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

    cLookupText = Compose . lookupText
    cLookupBs = Compose . lookupBs
    cLookupInt = Compose . lookupInt
