{-# LANGUAGE OverloadedStrings #-}
module Config where
import           Data.Yaml
import           Database.PostgreSQL.Simple
import           Logger
import           Network.Wai.Handler.Warp       ( Port )

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
