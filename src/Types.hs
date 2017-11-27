{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Types where

import Control.Lens
import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import System.Log.Raven.Types

import Configuration
import Logging
import Monitoring.Sentry.Configuration

newtype AppContextT m a = AppContextT { unAppT :: (ReaderT Config) (LoggingT m) a }
                                      deriving ( Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadLogger
                                                , MonadLoggerIO, MonadThrow, MonadCatch)

runAppT :: MonadIO m => AppContextT m a -> Config -> m a
runAppT (AppContextT m) c = (runAppLoggerT $ c ^. sentryService) (runReaderT m c)

instance MonadTrans AppContextT where
  lift = AppContextT . lift . lift

instance MonadBase b m => MonadBase b (AppContextT m) where
  liftBase = liftBaseDefault

instance MonadTransControl AppContextT where
  type StT AppContextT a = a
  liftWith = defaultLiftWith2 AppContextT unAppT
  restoreT = defaultRestoreT2 AppContextT

instance MonadBaseControl b m => MonadBaseControl b (AppContextT m) where
  type StM (AppContextT m) a = ComposeSt AppContextT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

type AppContext = AppContextT IO

runApp :: AppContext a -> Config -> IO a
runApp = runAppT

