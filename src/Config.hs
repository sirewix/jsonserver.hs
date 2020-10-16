module Config where
import           App.Prototype.Log              ( Priority(..) )
import           Data.Yaml                      ( (.!=)
                                                , (.:?)
                                                , FromJSON(..)
                                                , Object
                                                , Parser
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import           Network.Wai.Handler.Warp       ( Port )

newtype DBConfig = DBConfig { unDBConfig :: ConnectInfo }

instance FromJSON DBConfig where
  parseJSON = withObject "DBConfig" $ \v ->
    DBConfig <$> (ConnectInfo
      <$> v .:? "host"     .!= "localhost"
      <*> v .:? "port"     .!= 5432
      <*> v .:? "user"     .!= "postgres"
      <*> v .:? "password" .!= ""
      <*> v .:? "database" .!= "postgres")

data Config = Config
  { database                :: DBConfig
  , port                    :: Port
  , number_of_secrets       :: Int
  , secrets_update_interval :: Integer
  , backdoor                :: Bool
  , log_level               :: Priority
  , log_file                :: Maybe String
  , db_refresh_interval     :: Maybe Int
  , page_sizes              :: PageSizes
  }

instance FromJSON Config where
  parseJSON = withObject "Configuration" $ \v ->
    Config
      <$> v .:: "postgres"
      <*> v .:? "port"                    .!= 3000
      <*> v .:? "number_of_secrets"       .!= 2
      <*> v .:? "secrets_update_interval" .!= 60 -- min
      <*> v .:? "backdoor"                .!= False
      <*> v .:? "log_level"               .!= Warning
      <*> v .:? "log_file"
      <*> v .:? "db_refresh_interval"     .!= Just 1000 -- ms
      <*> v .:: "page_sizes"

(.::) :: FromJSON a => Object -> Text -> Parser a
v .:: name = v .:? name .!= object [] >>= parseJSON

data PageSizes = PageSizes
  { users      :: Int
  , authors    :: Int
  , posts      :: Int
  , comments   :: Int
  , categories :: Int
  , tags       :: Int
  }

instance FromJSON PageSizes where
  parseJSON = withObject "Page Sizes" $ \v ->
    PageSizes
      <$> v .:? "users"      .!= 20
      <*> v .:? "authors"    .!= 20
      <*> v .:? "posts"      .!= 20
      <*> v .:? "comments"   .!= 20
      <*> v .:? "categories" .!= 20
      <*> v .:? "tags"       .!= 20
