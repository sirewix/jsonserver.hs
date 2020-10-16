{-# LANGUAGE
    DeriveGeneric
  , DeriveAnyClass
  , DuplicateRecordFields
  , QuasiQuotes
  #-}

module Model.Comments where
import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Database         ( DbAccess
                                                , ToRow
                                                , Only(..)
                                                , Paged
                                                , Page
                                                , Id
                                                , execOne
                                                , queryOne
                                                , queryPaged
                                                , limit
                                                , offset
                                                , sql
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           Config                         ( Config(..)
                                                , PageSizes(..)
                                                )
import           GHC.Generics                   ( Generic )
import           Misc                           ( fromJson )
import           Model.Users                    ( UserFull(..) )

data CommentEssential = CommentEssential
  { post_id  :: Id
  , username :: Text
  , comment  :: Text
  } deriving (Generic, ToRow)

data CommentFull = CommentFull
  { id      :: Id
  , comment :: Text
  , user    :: UserFull
  } deriving Generic

instance FromJSON CommentFull
instance ToJSON CommentFull

getComments
  :: (DbAccess m, HasEnv Config m)
  => Id
  -> Page
  -> m (Paged CommentFull)
getComments pid page = do
  pageSize <- comments . page_sizes <$> getEnv
  mapM (fromJson . fromOnly) =<< queryPaged
    pageSize
    [sql|
      SELECT
          count(*) OVER(),
          json_build_object (
              'id', id,
              'comment', comment,
              'user', json_build_object(
                  'name', name,
                  'lastname', last_name,
                  'admin', admin,
                  'avatar', avatar,
                  'registration_date', registration_date
              )
          )
      FROM comments
      JOIN users ON user_id = users.id
      WHERE post = ?
      LIMIT ?
      OFFSET ?
    |]
    (pid, limit pageSize, offset pageSize page)

addComment
  :: (DbAccess m)
  => CommentEssential
  -> m (Either Text Id)
addComment = queryOne [sql|
    INSERT INTO comments (post, username, comment)
    VALUES (?, (SELECT id FROM users WHERE name = ?), ?)
    RETURNING id"
  |]

deleteComment
  :: (DbAccess m)
  => Id
  -> m (Either Text ())
deleteComment id = execOne "DELETE FROM comments WHERE id = ?" [id]
