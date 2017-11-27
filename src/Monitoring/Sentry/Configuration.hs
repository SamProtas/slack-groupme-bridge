
module Monitoring.Sentry.Configuration where

import Control.Lens
import System.Log.Raven
import System.Log.Raven.Transport.HttpConduit
import System.Log.Raven.Types


fromDsn :: String -> IO SentryService
fromDsn dsn = initRaven dsn id sendRecord errorFallback

class HasSentryService c where
  sentryService :: Lens' c SentryService

instance HasSentryService SentryService where
  sentryService = id
