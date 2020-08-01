{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Auth
import           Control.Concurrent
import           Control.Monad
import           Config
import           Data.Char
import           Data.Function
import           Data.Functor
import           Data.Yaml
import           Database.PostgreSQL.Simple
import           Entry
import           Logger
import           Misc
import           Network.Wai.Handler.Warp       ( run )
import           System.Environment
import           System.IO
import qualified Data.ByteString               as B

import GHC.Generics

main :: IO ()
main = do
  args <- getArgs
  configContents <- if null args
     then return "{}"
     else B.readFile (head args)

  config <- decodeThrow configContents
  hlog <- maybe (pure stdout) (\f -> openFile f WriteMode) (log_file config)
  log <- newMVar hlog <&> \out -> newLogger out (log_level config)

  db <- connect (database config)
  log Info $ "Starting server at http://localhost:" <> showText (port config)
  if backdoor config
    then log Warning $ "Backdoor is on, all token verification is off"
    else pure ()

  keys    <- replicateM (number_of_secrets config) generateSecret
  secrets <- newMVar keys
  void . forkIO $ do
    threadDelay $ 1000000 * 60 * (secrets_update_interval config)
    log Info $ "Updating keys"
    updateSecrets secrets
  run (port config) (app config secrets (log, db))
  log Info $ "Stopping"
