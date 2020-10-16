{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  #-}

module API.Search (posts) where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Database         ( DbAccess(..)
                                                , paginate
                                                )
import           Config                         ( Config )
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import           Data.Text                      ( Text )
import           Query.Common                   ( Page(..) )
import           Query.FromQuery                ( FromQuery(..)
                                                , opt
                                                , optT
                                                )
import qualified Model.Search                  as M

data Search = Search
  { sortBy      :: Sort
  , tagsInAll   :: TagsInAll
  , createdAt   :: CreatedAt
  , author      :: Maybe Text
  , category_id :: Maybe Int
  , title       :: Maybe Text
  , content     :: Maybe Text
  , everywhere  :: Maybe Text
  }

instance FromQuery Search where
  parseQuery = Search
    <$> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> opt "author"
    <*> optT "category_id"
    <*> opt "title"
    <*> opt "content"
    <*> opt "search"

newtype CreatedAt = CreatedAt { unCreatedAt :: M.CreatedAt }

instance FromQuery CreatedAt where
  parseQuery = CreatedAt <$> do
    createdAt <- optT "created_at"
    case createdAt of
      Just a -> pure (M.CreatedAt a)
      Nothing -> M.CreatedAtRange
         <$> optT "created_at__gt"
         <*> optT "created_at__lt"

newtype TagsInAll = TagsInAll { unTagsInAll :: M.TagsInAll }

instance FromQuery TagsInAll where
  parseQuery = do
    tagsIn  <- fromMaybe [] <$> optT "tags__in"
    tagsAll <- fromMaybe [] <$> optT "tags__all"
    justTag <- maybeToList <$> optT "tag"
    return . TagsInAll $ M.TagsInAll tagsIn (tagsAll ++ justTag)

newtype Sort = Sort { unSort :: M.Sort }

instance FromQuery Sort where
  parseQuery = fmap Sort $ M.Sort
    <$> (fromMaybe M.ByDate <$> optT "sort")
    <*> (fromMaybe False <$> optT "sort_reversed")

posts
  :: (DbAccess m, HasEnv Config m)
  => (Search, Page)
  -> m AppResponse
posts (Search{..}, Page page) = AppOk . paginate <$> M.search
  page
  (unSort sortBy)
  (unTagsInAll tagsInAll)
  (unCreatedAt createdAt)
  author
  category_id
  title
  content
  everywhere

