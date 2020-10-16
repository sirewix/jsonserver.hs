{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , DeriveAnyClass
  , DuplicateRecordFields
  , FlexibleContexts
  , QuasiQuotes
  , ScopedTypeVariables
  #-}

module Model.Users where

import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Auth             ( Secrets )
import           App.Prototype.Database         ( ToRow(..)
                                                , DbAccess(..)
                                                , Only(..)
                                                , Page
                                                , Paged(..)
                                                , (:.)(..)
                                                , execOne
                                                , limit
                                                , offset
                                                , queryPaged
                                                , sql
                                                )
import           App.Implementation.Auth        ( generateJWT, runJWT )
import           Config                         ( Config(..)
                                                , PageSizes(..)
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Date                      ( Date )
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import           Misc                           ( fromJson )

data UserEssential = UserEssential
  { name     :: Text
  , lastname :: Text
  , admin    :: Bool
  , avatar   :: Maybe Text
  } deriving (Generic, ToRow)

data UserFull = UserFull
  { name     :: Text
  , lastname :: Text
  , admin    :: Bool
  , avatar   :: Maybe Text
  , registration_date :: Date
  } deriving Generic

instance FromJSON UserFull
instance ToJSON UserFull

getUsers
  :: (DbAccess m, HasEnv Config m)
  => Page
  -> m (Paged UserFull)
getUsers page = do
  pageSize <- users . page_sizes <$> getEnv
  mapM (fromJson . fromOnly) =<< queryPaged
    pageSize
    [sql|
      SELECT
          count(*) OVER(),
          json_build_object (
              'name', name,
              'lastname', lastName,
              'admin', admin,
              'avatar', avatar,
              'registration_date', registrationDate
          )
      FROM users
      LIMIT ?
      OFFSET ?
    |]
    (limit pageSize, offset pageSize page)

createUser
  :: DbAccess m
  => UserEssential
  -> Text
  -> m (Either Text ())
createUser user password = execOne
  "INSERT INTO users (name, lastname, admin, avatar, password) VALUES (?, ?, ?, ?, ?)"
  (user :. [password])

deleteUser
  :: DbAccess m
  => Text
  -> m (Either Text ())
deleteUser username = execOne "DELETE FROM users WHERE name = ?" [username]

data Credentials = Credentials
  { name     :: Text
  , password :: Text
  } deriving (Generic, ToRow)

getUserToken
  :: ( DbAccess m
     , HasEnv Config m
     , HasEnv Secrets m
     , MonadIO m
     )
  => Credentials
  -> m (Either Text Text)
getUserToken (creds@Credentials {..}) = do
  res <- query [sql|
    SELECT users.admin, authors.id
      FROM users
      LEFT OUTER JOIN authors ON authors.user_id = users.id
      WHERE name = ? AND password = ?
    |] creds
  case res of
    Right [(Just isAdmin, isAuthor :: Maybe Int)] ->
      Right . generateJWT (isAdmin, isJust isAuthor, name) <$> runJWT
    _ -> return $ Left "access denied"
