{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Types where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control

import Configuration

newtype AppContextT m a = AppContextT { unAppT :: (ReaderT Config) (LoggingT m) a }
                                      deriving ( Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadLogger
                                                , MonadLoggerIO)

runAppT :: MonadIO m => AppContextT m a -> Config -> m a
runAppT (AppContextT m) c = runStdoutLoggingT (runReaderT m c)

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

askConfig :: (MonadReader r m, HasConfig r) => m Config
askConfig = view config <$> ask

askGroupMeConfig :: (MonadReader r m, HasGroupMeConfig r) => m GroupMeConfig
askGroupMeConfig = view groupMeConfig <$> ask

askSlackConfig :: (MonadReader r m, HasSlackConfig r) => m SlackConfig
askSlackConfig = view slackConfig <$> ask
