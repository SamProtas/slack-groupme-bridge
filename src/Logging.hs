module Logging where

import Control.Monad.IO.Class
import System.IO

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Monad.Logger
import System.Log.FastLogger
import System.Log.Raven.Types


import Monitoring.Sentry


runAppLoggerT :: (MonadIO m) => SentryService -> LoggingT m a -> m a
runAppLoggerT service m = runLoggingT m (logger service)

sentryPredicate :: LogLevel -> Bool
sentryPredicate lvl = lvl == LevelError || lvl == LevelWarn

logger service = combineLoggers
                   (bothLoggers (sentryLoggerFn service) stdOutLoggerFn)
                   sentryPredicate
                   stdOutLoggerFn

type LogFnArgs = Loc -> LogSource -> LogLevel -> LogStr
type LogFn = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

sentryLoggerFn service loc src lvl str = do
  let mdl = loc_module loc
      lvl' = lvlToSentryLevel lvl
      str' = C.unpack $ fromLogStr str
  sentryLog service lvl' str' mdl str'

runSentryLoggingT :: (MonadIO m) => SentryService -> LoggingT m a -> m a
runSentryLoggingT service m = runLoggingT m (sentryLoggerFn service)



lvlToSentryLevel :: LogLevel -> SentryLevel
lvlToSentryLevel lvl = case lvl of LevelDebug -> Debug
                                   LevelInfo -> Info
                                   LevelWarn -> Warning
                                   LevelError -> Error
                                   LevelOther o -> Custom $ T.unpack o


type LevelPredicate = LogLevel -> Bool

combineLoggers :: LogFn -- ^. If predicate is true, use this
               -> LevelPredicate
               -> LogFn -- ^. If predicate is false, use this
               -> Loc
               -> LogSource
               -> LogLevel
               -> LogStr
               -> IO ()
combineLoggers ifTrueLogger predicate defaultLogger loc src lvl str =
  if predicate lvl then ifTrueLogger loc src lvl str else defaultLogger loc src lvl str

bothLoggers :: LogFn -> LogFn -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
bothLoggers fn1 fn2 loc src lvl str = fn1 loc src lvl str >> fn2 loc src lvl str

-- Taken from Monad-Logger.... wish this was exported
stdOutLoggerFn = defaultOutput stdout

defaultOutput :: Handle
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
defaultOutput h loc src level msg =
    C.hPutStr h ls
    where ls = defaultLogStrBS loc src level msg

defaultLogStrBS :: Loc
                -> LogSource
                -> LogLevel
                -> LogStr
                -> C.ByteString
defaultLogStrBS a b c d =
    toBS $ defaultLogStr a b c d
  where toBS = fromLogStr

