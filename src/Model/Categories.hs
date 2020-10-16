{-# LANGUAGE
    DeriveGeneric
  , DeriveAnyClass
  , DuplicateRecordFields
  , QuasiQuotes
  #-}

module Model.Categories where

import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Database         ( (:.)(..)
                                                , DbAccess(..)
                                                , Id
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
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Misc                           ( fromJson )

data CategoryEssential = CategoryEssential
  { name      :: Text
  , parent_id :: Maybe Id
  } deriving (Generic, ToRow)

data CategoryPartial = CategoryPartial
  { name      :: Maybe Text
  , parent_id :: Maybe (Maybe Id)
  } deriving (Generic, ToRow)

data CategoryFull = CategoryFull
  { id        :: Id
  , name      :: Text
  , parent_id :: Maybe Id
  } deriving Generic

instance FromJSON CategoryFull
instance ToJSON CategoryFull

getCategories
  :: (DbAccess m, HasEnv Config m)
  => Maybe Id
  -> Page
  -> m (Paged CategoryFull)
getCategories mbParentId page = do
  pageSize <- categories . page_sizes <$> getEnv
  let q = [sql|
            SELECT
              count(*) OVER(),
              to_json(categories)
            FROM categories WHERE
          |]
      cond = case mbParentId of
        Just _  -> "parent_id = ?"
        Nothing -> "parent_id is ?"
  mapM (fromJson . fromOnly) =<< queryPaged
    pageSize
    (q <> " " <> cond <> " LIMIT ? OFFSET ?")
    (mbParentId, limit pageSize, offset pageSize page)

createCategory
  :: (DbAccess m)
  => CategoryEssential
  -> m (Either Text Id)
createCategory = queryOne "INSERT INTO categories (name, parent_id) VALUES (?, ?) RETURNING id"

editCategory
  :: (DbAccess m)
  => Id
  -> CategoryPartial
  -> m (Either Text ())
editCategory id entity = execOne [sql|
    UPDATE categories
    SET name      = COALESCE (?, name),
    SET parent_id = COALESCE (?, parent_id)
    WHERE id = ?
  |] (entity :. [id])

deleteCategory
  :: (DbAccess m)
  => Id
  -> m (Either Text ())
deleteCategory id = execOne "DELETE FROM categories WHERE id = ?" [id]
