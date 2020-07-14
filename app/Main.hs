{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
, DeriveGeneric
#-}

module Main where
import           Auth
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
  { database :: ConnectInfo
  , port :: Port
  , number_of_secrets :: Int
  , secrets_update_interval :: Int
  , backdoor :: Bool
  } --deriving Generic
instance FromJSON Config where
  parseJSON = withObject "Configuration" $ \v ->
    Config
      <$> ((v .:? "postgres" .!= object []) >>= parseJSON)
      <*> v .:? "port"                    .!= 3000
      <*> v .:? "number_of_secrets"       .!= 2
      <*> v .:? "secrets_update_interval" .!= 60 -- min
      <*> v .:? "backdoor"                .!= False

main :: IO ()
main = do
  args <- getArgs
  configContents <- if null args
     then return "{}"
     else B.readFile (head args)

  config <- decodeThrow configContents

  db <- connect (database config)
  Prelude.putStrLn $ "http://localhost:" <> show (port config)
  keys    <- replicateM (number_of_secrets config) generateSecret
  secrets <- newMVar keys
  void . forkIO $ do
    threadDelay $ 1000000 * 60 * (secrets_update_interval config)
    updateSecrets secrets
  run (port config) (app (backdoor config) secrets db)
