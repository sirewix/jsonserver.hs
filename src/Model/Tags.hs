{-# LANGUAGE
    DeriveGeneric
  , DeriveAnyClass
  , QuasiQuotes
  #-}

module Model.Tags where

import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Database         ( (:.)(..)
                                                , DbAccess
                                                , ToRow
                                                , Only(..)
                                                , Paged
                                                , Page
                                                , Id(..)
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
import           Config                         ( Config(..)
                                                , PageSizes(..)
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Misc                           ( fromJson )

data Tag

newtype TagEssential = TagEssential Text
  deriving (Generic, ToRow)

data TagFull = TagFull
  { id :: Id Tag
  , tag :: Text
  } deriving Generic

instance FromJSON TagFull
instance ToJSON TagFull

getTags
  :: (DbAccess m, HasEnv Config m)
  => Page
  -> m (Paged TagFull)
getTags page = do
  pageSize <- tags . page_sizes <$> getEnv
  mapM (fromJson . fromOnly) =<< queryPaged
    pageSize
    [sql|
      SELECT
          count(*) OVER(),
          to_json(tags)
      FROM tags LIMIT ? OFFSET ?
    |]
    (limit pageSize, offset pageSize page)

createTag
  :: (DbAccess m)
  => TagEssential
  -> m (Either Text (Id Tag))
createTag = queryOne "INSERT INTO tags (tag) VALUES (?) RETURNING id"

editTag
  :: (DbAccess m)
  => (Id Tag)
  -> TagEssential
  -> m (Either Text ())
editTag id tag = execOne "UPDATE tags SET tag = ? WHERE id = ?" (tag :. [id])

deleteTag
  :: (DbAccess m)
  => (Id Tag)
  -> m (Either Text ())
deleteTag id = execOne "DELETE FROM tags WHERE id = ?" [id]
