{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
, DeriveGeneric
#-}

module Main where
import           Auth
import           Misc
import           Control.Concurrent
import           Control.Monad
import           Data.Char
import           Data.Function
import           Database.PostgreSQL.Simple
import           Entry
import           Network.Wai.Handler.Warp       ( Port, run )
import           System.Environment
import           Data.Yaml
import           Data.Functor
import qualified Data.ByteString as B
import           Logger
import           System.IO

import GHC.Generics

instance FromJSON ConnectInfo where
  parseJSON = withObject "ConnectInfo" $ \v ->
    ConnectInfo
      <$> v .:? "host"     .!= "localhost"
      <*> v .:? "port"     .!= 5432
      <*> v .:? "user"     .!= "postgres"
      <*> v .:? "password" .!= ""
      <*> v .:? "database" .!= "postgres"

data Config = Config
  { database                :: ConnectInfo
  , port                    :: Port
  , number_of_secrets       :: Int
  , secrets_update_interval :: Int
  , backdoor                :: Bool
  , log_level               :: Priority
  , log_file                :: Maybe String
  } --deriving Generic
instance FromJSON Config where
  parseJSON = withObject "Configuration" $ \v ->
    Config
      <$> ((v .:? "postgres" .!= object []) >>= parseJSON)
      <*> v .:? "port"                    .!= 3000
      <*> v .:? "number_of_secrets"       .!= 2
      <*> v .:? "secrets_update_interval" .!= 60 -- min
      <*> v .:? "backdoor"                .!= False
      <*> v .:? "log_level"               .!= Warning
      <*> v .:? "log_file"

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
  if backdoor config then
      log Warning $ "Backdoor is on, all token verification is off"
  else pure ()

  keys    <- replicateM (number_of_secrets config) generateSecret
  secrets <- newMVar keys
  upd_thread <- forkIO $ do
    threadDelay $ 1000000 * 60 * (secrets_update_interval config)
    log Info $ "Updating keys"
    updateSecrets secrets
  run (port config) (app (backdoor config) secrets (log, db))
  log Info $ "Stopping"
  killThread upd_thread
