{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}
module App.Implementation.Log where

import           App.Prototype.Log              ( Priority(..) )
import           Control.Concurrent             ( MVar
                                                , withMVar
                                                )
import           Control.Monad                  ( when )
import           Data.Text.IO                   ( hPutStrLn )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( getCurrentTime )
import           Misc                           ( showText )
import qualified System.IO

data LogEnv = LogEnv Priority (MVar System.IO.Handle)

hlog :: LogEnv -> (Priority, Text) -> IO ()
hlog (LogEnv logPrio h) (prio, msg) = when (prio >= logPrio) $ do
  now <- getCurrentTime
  withMVar h $ \h -> hPutStrLn h $ showText now <> " [" <> showText prio <> "] " <> msg
