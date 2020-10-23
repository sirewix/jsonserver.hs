{-# LANGUAGE
    DeriveGeneric
  , DeriveAnyClass
  , DuplicateRecordFields
  , QuasiQuotes
  #-}

module Model.Posts where

import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Database         ( (:.)(..)
                                                , DbAccess(..)
                                                , Id(..)
                                                , PGArray(..)
                                                , Only(..)
                                                , Page
                                                , Paged
                                                , ToRow(..)
                                                , execOne
                                                , queryOne
                                                , queryPaged
                                                , limit
                                                , offset
                                                , sql
                                                )
import           Config                         ( Config(..)
                                                , PageSizes(..)
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Date                      ( Date(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Misc                           ( fromJson )
import           Model.Categories               ( Category
                                                , CategoryFull
                                                )
import           Model.Tags                     ( Tag
                                                , TagFull
                                                )
import           Model.Authors                  ( AuthorFull )

data Post

data PostEssential = PostEssential
  { title       :: Text
  , category_id :: Id Category
  , content     :: Text
  , main_image  :: Text
  , images      :: PGArray Text
  } deriving (Generic, ToRow)

data PostPartial = PostPartial
  { title       :: Maybe Text
  , category_id :: Maybe (Id Category)
  , content     :: Maybe Text
  , main_image  :: Maybe Text
  , images      :: Maybe (PGArray Text)
  } deriving (Generic, ToRow)


data PostFull = PostFull
  { id            :: Id Post
  , title         :: Text
  , date          :: Date
  , category_tree :: [CategoryFull]
  , content       :: Text
  , main_image    :: Text
  , images        :: [Text]
  , tags          :: [TagFull]
  , author        :: AuthorFull
  } deriving Generic

instance FromJSON PostFull
instance ToJSON PostFull

getPublishedPost
  :: DbAccess m
  => Id Post
  -> m (Either Text PostFull)
getPublishedPost pid = mapM fromJson =<< queryOne [sql|
    SELECT json
    FROM posts_view
    WHERE id = ? AND published = true
  |] [pid]

getUnpublishedPost
  :: DbAccess m
  => Id Post
  -> Text
  -> m (Either Text PostFull)
getUnpublishedPost pid author = mapM fromJson =<< queryOne [sql|
    SELECT json
    FROM posts_view
    WHERE id = ? AND (json->'author'->>'username') = ?
  |] (pid, author)

getDrafts
  :: (DbAccess m, HasEnv Config m)
  => Text
  -> Page
  -> m (Paged PostFull)
getDrafts author page = do
  pageSize <- posts . page_sizes <$> getEnv
  mapM (fromJson . fromOnly) =<< queryPaged
    pageSize
    [sql|
      SELECT
          count(*) OVER(),
          json
      FROM posts_view
      WHERE (json->'author'->>'username') = ?
      LIMIT ?
      OFFSET ?
    |]
    (author, limit pageSize, offset pageSize page)

createPost
  :: (DbAccess m)
  => Text
  -> PostEssential
  -> m (Either Text (Id Post))
createPost author entity = queryOne [sql|
    INSERT INTO posts (title, category, content, main_image, images, author)
    VALUES (?, ?, ?, ?, ?, author_id_by_username(?))
    RETURNING id
  |] (entity :. [author])

data TagPostRelation = TagPostRelation
  { tag_id  :: Id Tag
  , post_id :: Id Post
  } deriving (Generic, ToRow)

attachTag
  :: DbAccess m
  => TagPostRelation
  -> Text
  -> m (Either Text ())
attachTag relation author = execOne
  [sql|
    INSERT INTO tag_post_relations (tag, post) (
      SELECT ?, id
      FROM posts
      WHERE id = ? AND author = author_id_by_username(?)
    )
  |] (relation :. [author])

deattachTag
  :: DbAccess m
  => TagPostRelation
  -> Text
  -> m (Either Text ())
deattachTag relation author = execOne
  [sql|
    DELETE FROM tag_post_relations
    WHERE tag = ? AND post = (
      SELECT id
      FROM posts
      WHERE id = ? AND author = author_id_by_username(?)
    )
  |] (relation :. [author])


editPost
  :: DbAccess m
  => Id Post
  -> Text
  -> PostPartial
  -> m (Either Text ())
editPost id author entity = execOne
  [sql|
    UPDATE posts SET
      title      = COALESCE (?, title),
      category   = COALESCE (?, category),
      content    = COALESCE (?, content),
      main_image = COALESCE (?, main_image),
      images     = COALESCE (?, images)
    WHERE id = ? AND author = author_id_by_username(?)
  |] (entity :. [id] :. [author])

publishPost
  :: DbAccess m
  => Text
  -> Id Post
  -> m (Either Text ())
publishPost author pid = execOne
  [sql|
    UPDATE posts
    SET published = true
    WHERE id = ? AND author = author_id_by_username(?)
  |] (pid, author)

deletePost
  :: DbAccess m
  => Text
  -> Id Post
  -> m (Either Text ())
deletePost author pid = execOne
  [sql|
    DELETE FROM posts
    WHERE id = ? AND author = author_id_by_username(?)
  |] (pid, author)
