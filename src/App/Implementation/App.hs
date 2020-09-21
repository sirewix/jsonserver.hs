{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  , OverloadedStrings
  , DeriveFunctor
  , MultiParamTypeClasses
  #-}
module App.Implementation.App where

import           App.Implementation.Database    ( DbEnv(..)
                                                , postgresqlExecute
                                                , postgresqlQuery
                                                )
import           App.Implementation.Log         ( LogEnv(..)
                                                , hlog
                                                )
import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Auth             ( Secrets )
import           App.Prototype.Database         ( DbAccess(..) )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
import           Config                         ( Config(..) )
import           Control.Monad.Except           ( ExceptT(..)
                                                , MonadError(..)
                                                , runExceptT
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( ReaderT(..)
                                                , MonadReader(..)
                                                )
import           Data.Text                      ( Text )

data Env = Env
  { logEnv  :: LogEnv
  , dbEnv   :: DbEnv
  , secrets :: Secrets
  , config  :: Config
  }

newtype App a = App (ReaderT Env (ExceptT Text IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError Text)

instance HasEnv LogEnv  App where getEnv = reader logEnv
instance HasEnv DbEnv   App where getEnv = reader dbEnv
instance HasEnv Secrets App where getEnv = reader secrets
instance HasEnv Config  App where getEnv = reader config

instance HasLog App where
  log' prio msg = getEnv >>= liftIO . (`hlog` (prio, msg))

instance DbAccess App where
  execute = postgresqlExecute
  query = postgresqlQuery

runApp :: App a -> Env -> IO (Either Text a)
runApp (App app) env  = runExceptT $ runReaderT app env

runLoggedApp :: App a -> Env -> a -> IO a
runLoggedApp app env def = runApp app env >>= either handleError return
  where handleError msg = hlog (logEnv env) (Error, msg) >> return def
