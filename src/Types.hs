{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Types where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control

import Configuration

newtype AppContextT m a = AppContextT { unAppT :: (ReaderT Config) m a }
                                      deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadTrans)

runAppT :: AppContextT m a -> Config -> m a
runAppT (AppContextT m) = runReaderT m

type AppContext2 = AppContextT IO

instance MonadBase b m => MonadBase b (AppContextT m) where
  liftBase = liftBaseDefault

instance MonadTransControl AppContextT where
  type StT AppContextT a = a
  liftWith = defaultLiftWith AppContextT unAppT
  restoreT = defaultRestoreT AppContextT

instance MonadBaseControl b m => MonadBaseControl b (AppContextT m) where
  type StM (AppContextT m) a = ComposeSt AppContextT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

type AppContext = AppContextT IO

runApp :: AppContext a -> Config -> IO a
runApp (AppContextT m) = runReaderT m

askConfig :: (MonadReader r m, HasConfig r) => m Config
askConfig = view config <$> ask

askGroupMeConfig :: (MonadReader r m, HasGroupMeConfig r) => m GroupMeConfig
askGroupMeConfig = view groupMeConfig <$> ask

askSlackConfig :: (MonadReader r m, HasSlackConfig r) => m SlackConfig
askSlackConfig = view slackConfig <$> ask
