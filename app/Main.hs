{-# LANGUAGE OverloadedStrings #-}

module Main where
import           App
import           Auth
import           Config
import           Control.Concurrent
import           Control.Monad
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

main :: IO ()
main = do
  args           <- getArgs
  configContents <- if null args then return "{}" else B.readFile (head args)

  config         <- decodeThrow configContents
  hlog           <- maybe (pure stdout) (\f -> openFile f WriteMode) (log_file config)
  log            <- newMVar hlog <&> \out -> newLogger out (log_level config)

  db             <- connect (unDBConfig $ database config)
  log Info $ "Starting server at http://localhost:" <> showText (port config)
  if backdoor config
    then log Warning "Backdoor is on, all token verification is off"
    else pure ()

  keys    <- replicateM (number_of_secrets config) generateSecret
  secrets <- newMVar keys

  let schedule ms msg action = void . forkIO . forever $ do
        threadDelay (1000 * ms)
        case msg of
          Just msg -> log Info msg
          Nothing  -> return ()
        action

  schedule (1000 * 60 * secrets_update_interval config)
           (Just "Updating keys")
           (updateSecrets secrets)

  case db_refresh_interval config of
    Nothing -> log Warning
      "Database refreshing is off, make sure the database is being refreshed somehow"
    Just interval -> schedule interval Nothing (dbrefresh db)

  run (port config) (app config secrets (log, db))
  log Info $ "Stopping"
