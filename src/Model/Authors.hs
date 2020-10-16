{-# LANGUAGE
    DeriveGeneric
  , DeriveAnyClass
  , DuplicateRecordFields
  , QuasiQuotes
  #-}

module Model.Authors where

import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Database         ( (:.)(..)
                                                , DbAccess
                                                , ToRow
                                                , Only(..)
                                                , Paged
                                                , Page
                                                , Id
                                                , execOne
                                                , queryPaged
                                                , limit
                                                , offset
                                                , sql
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Config                         ( Config(..)
                                                , PageSizes(..)
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Misc                           ( fromJson )

data AuthorEssential = AuthorEssential
  { username    :: Text
  , description :: Text
  } deriving (Generic, ToRow)

data AuthorPartial = AuthorPartial
  { username    :: Maybe Text
  , description :: Maybe Text
  } deriving (Generic, ToRow)

data AuthorFull = AuthorFull
  { id          :: Id
  , username    :: Text
  , description :: Text
  } deriving Generic

instance FromJSON AuthorFull
instance ToJSON AuthorFull

getAuthors
  :: (DbAccess m, HasEnv Config m)
  => Page
  -> m (Paged AuthorFull)
getAuthors page = do
  pageSize <- authors . page_sizes <$> getEnv
  mapM (fromJson . fromOnly) =<< queryPaged
    pageSize
    [sql|
      SELECT
          count(*) OVER(),
          json_build_object (
            'id', authors.id,
            'username', users.name,
            'description', authors.description
          )
      FROM authors
      JOIN users ON authors.user_id = users.id
      LIMIT ?
      OFFSET ?
    |]
    (limit pageSize, offset pageSize page)

makeAuthor
  :: (DbAccess m)
  => AuthorEssential
  -> m (Either Text ())
makeAuthor = execOne [sql|
    INSERT
    INTO authors (user_id, description)
    VALUES ((SELECT id FROM users WHERE name = ?), ?)
  |]


editAuthor
  :: (DbAccess m)
  => Text
  -> AuthorPartial
  -> m (Either Text ())
editAuthor username author = execOne [sql|
    UPDATE authors
    SET user_id = COALESCE ((SELECT id FROM users WHERE name = ?), user_id),
    SET description = COALESCE (?, description)
    WHERE user_id = (SELECT id FROM users WHERE name = ?)
  |] (author :. [username])

deleteAuthor
  :: (DbAccess m)
  => Text
  -> m (Either Text ())
deleteAuthor username = execOne "DELETE FROM authors WHERE username = ?" [username]
