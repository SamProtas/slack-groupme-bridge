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
            -> String -- ^. Module
            -> String -- ^. Value
            -> m ()
sentryLog service lvl msg mdl value = 
  liftIO $ register
             service
             "bridge.sentry.logger"
             lvl
             msg $
               exception value Nothing (Just mdl)

