module Bridge where

import Control.Lens
import Data.Monoid
import System.Exit

import Data.Text (Text)
import qualified Data.Text as T

import Configuration
import GroupMe.Server


bridge :: IO ()
bridge = do
  eitherConfig <- eitherGetConfig
  case eitherConfig of Left missing -> print $ "Missing config key: " <> missing
                       Right config -> runServer config
  exitFailure

