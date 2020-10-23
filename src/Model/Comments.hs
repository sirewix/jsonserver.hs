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
import           Model.Posts                    ( Post )

data Comment

data CommentEssential = CommentEssential
  { post_id  :: Id Post
  , username :: Text
  , comment  :: Text
  } deriving (Generic, ToRow)

data CommentFull = CommentFull
  { id      :: Id Comment
  , comment :: Text
  , user    :: UserFull
  } deriving Generic

instance FromJSON CommentFull
instance ToJSON CommentFull

getComments
  :: (DbAccess m, HasEnv Config m)
  => Id Post
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
              'id', comments.id,
              'comment', comment,
              'user', json_build_object(
                  'name', name,
                  'lastname', lastname,
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
  -> m (Either Text (Id Comment))
addComment = queryOne [sql|
    INSERT INTO comments (post, user_id, comment)
    VALUES (?, (SELECT id FROM users WHERE name = ?), ?)
    RETURNING id
  |]

deleteComment
  :: (DbAccess m)
  => Id Comment
  -> m (Either Text ())
deleteComment id = execOne "DELETE FROM comments WHERE id = ?" [id]
