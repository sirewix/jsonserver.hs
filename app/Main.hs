{-# LANGUAGE OverloadedStrings #-}

module Main where
import           App                            ( app )
import           App.Implementation.App         ( Env(..)
                                                , runLoggedApp
                                                )
import           App.Implementation.Auth        ( generateSecret
                                                , updateSecrets
                                                )
import           App.Implementation.Log         ( LogEnv(..)
                                                , hlog
                                                )
import           App.Implementation.Database    ( DbEnv(..) )
import           App.Prototype.Log              ( Priority(..) )
import           Config                         ( Config(..)
                                                , DBConfig(..)
                                                )
import           Control.Concurrent             ( newMVar
                                                , forkIO
                                                , threadDelay
                                                )
import           Control.Monad                  ( forever
                                                , forM_
                                                , replicateM
                                                )
import           Data.Functor                   ( void )
import           Data.Yaml                      ( decodeThrow )
import           Database.PostgreSQL.Simple     ( connect )
import           Entry                          ( entry
                                                , dbrefresh
                                                )
import           Misc                           ( showText )
import           Network.Wai.Handler.Warp       ( run )
import           System.Environment             ( getArgs )
import           System.IO                      ( stdout
                                                , openFile
                                                , IOMode(..)
                                                )
import qualified Data.ByteString               as B

main :: IO ()
main = do
  args           <- getArgs
  configContents <- if null args then return "{}" else B.readFile (head args)

  config         <- decodeThrow configContents
  logFileHandler <- maybe (pure stdout) (`openFile` WriteMode) (log_file config)
  logHandler <- newMVar logFileHandler
  let log = curry . hlog $ LogEnv (log_level config) logHandler

  conn           <- connect (unDBConfig $ database config)
  log Info $ "Starting server at http://localhost:" <> showText (port config)
  if backdoor config
    then log Warning "Backdoor is on, all token verification is off"
    else pure ()

  keys    <- replicateM (number_of_secrets config) generateSecret
  secrets <- newMVar keys

  let env = Env
        (LogEnv (log_level config) logHandler)
        (DbEnv conn)
        secrets
        config

  let schedule ms msg action = void . forkIO . forever $ do
        threadDelay (1000 * ms)
        forM_ msg (log Info)
        action

  schedule (fromInteger $ 1000 * 60 * secrets_update_interval config)
           (Just "Updating keys")
           (updateSecrets secrets)

  case db_refresh_interval config of
    Nothing -> log Warning
      "Database refreshing is off, make sure the database is being refreshed somehow"
    Just interval -> schedule interval Nothing $ runLoggedApp dbrefresh env ()

  run (port config) (app entry env)
  log Info "Stopping"
