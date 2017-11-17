module Bridge where

import Control.Lens
import Data.Monoid
import System.Exit

import Control.Concurrent.Async
import Data.Text (Text)
import qualified Data.Text as T

import Configuration
import GroupMe.Server
import Slack.Listener
import Types


bridge :: IO ()
bridge = do
  eitherConfig <- eitherGetConfig
  case eitherConfig of Left missing -> print $ "Missing config key: " <> missing
                       Right config -> race_ (runApp startWsListener config) (runApp runServer config)
  exitFailure

