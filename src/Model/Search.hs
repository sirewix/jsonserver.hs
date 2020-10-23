{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  #-}

module Model.Search where

import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Log              ( HasLog(..), Priority(..) )
import           App.Prototype.Database         ( DbAccess(..)
                                                , Query(..)
                                                , Id
                                                , PGArray(..)
                                                , Only(..)
                                                , Page
                                                , Paged
                                                , queryPaged
                                                , limit
                                                , offset
                                                , sql
                                                )
import           Config                         ( Config(..)
                                                , PageSizes(..)
                                                )
import           Data.Date                      ( Date(..) )
import           Data.Text                      ( Text )
import           Misc                           ( fromJson )
import           Model.Posts                    ( PostFull )
import           Model.Categories               ( Category )
import           Model.Tags                     ( Tag )
import           Data.Text.Encoding             ( decodeUtf8 )


data Sort = Sort SortBy Bool

data SortBy =
    ByDate
  | ByAuthor
  | ByCategory
  | ByNumberOfImages
  deriving (Read)

data TagsInAll = TagsInAll [Id Tag] [Id Tag]

data CreatedAt =
    CreatedAt Date
  | CreatedAtRange (Maybe Date) (Maybe Date)

search
  :: (DbAccess m, HasEnv Config m, HasLog m)
  => Page
  -> Sort
  -> TagsInAll
  -> CreatedAt
  -> Maybe Text
  -> Maybe (Id Category)
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> m (Paged PostFull)
search
  page
  sortBy
  (TagsInAll tagsIn tagsAll)
  createdAt
  mbAuthor
  mbCategoryId
  mbTitle
  mbContent
  mbEverywhere
  = do
  let q = [sql|
          SELECT DISTINCT ON (posts_view.id) count(*) OVER(), json
          FROM posts_view
          LEFT JOIN tag_post_relations ON post = posts_view.id
          LEFT JOIN tags ON tags.id = tag_post_relations.tag
          WHERE
              published = true
          AND COALESCE ((json->'author'->>'username') = ?, true)
          AND COALESCE (categoryid =                    ?, true)
          AND COALESCE ((json->>'title') ~~*            ?, true)
          AND COALESCE ((json->>'content') ~~*          ?, true)

          AND COALESCE (posts_view.tag_ids @>           ?, true)
          AND COALESCE (posts_view.tag_ids &&           ?, true)

          AND COALESCE (date =                          ?, true)
          AND COALESCE (date >                          ?, true)
          AND COALESCE (date <                          ?, true)

          AND( COALESCE ((json->>'content') ~~*            ?, true)
            OR COALESCE ((json->>'title') ~~*              ?, true)
            OR COALESCE ((json->'author'->>'username') ~~* ?, true)
            OR COALESCE (categoryname ~~*                  ?, true)
            OR COALESCE (COALESCE(tags.tag, '') ~~*        ?, true)
             )
          ORDER BY posts_view.id,
          |]
          <> orderBy sortBy
          <> " LIMIT ? OFFSET ? "
  log' Debug $ "Build query: " <> decodeUtf8 (fromQuery q)
  pageSize <- posts . page_sizes <$> getEnv
  mapM (fromJson . fromOnly) =<< queryPaged pageSize q
      ( mbAuthor
      , mbCategoryId
      , anywhere <$> mbTitle
      , anywhere <$> mbContent

      , PGArray <$> nullify tagsIn
      , PGArray <$> nullify tagsAll

      , dateEq
      , dateGt
      , dateLt

      , search
      , search
      , search
      , search
      , search

      , limit pageSize
      , offset pageSize page
      )
 where
  nullify list = if null list then Nothing else Just list
  search = anywhere <$> mbEverywhere
  (dateEq, dateGt, dateLt) =
    case createdAt of
      CreatedAt date -> (Just date, Nothing, Nothing)
      CreatedAtRange from to -> (Nothing, from, to)
  anywhere text = "%" <> text <> "%"

orderBy :: Sort -> Query
orderBy (Sort sort rev) = f sort <> if rev then " ASC " else " DESC "
  where
    f ByDate           = "date"
    f ByAuthor         = "(json->'author'->>'username')"
    f ByCategory       = "categoryname"
    f ByNumberOfImages = "numberofimages"

