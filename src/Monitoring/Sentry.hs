module Monitoring.Sentry where


import Control.Lens
import Control.Monad.Reader
import System.Log.Raven
import System.Log.Raven.Interfaces
import System.Log.Raven.Transport.HttpConduit
import System.Log.Raven.Types

import Configuration
import Monitoring.Sentry.Configuration


sentryLog :: (MonadIO m)
            => SentryService
            -> SentryLevel
            -> String -- ^. Message
            -> m ()
sentryLog service lvl msg = 
  liftIO $ register
             service
             "bridge.sentry.logger"
             lvl
             msg
             id

