{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  #-}

module Model.Search where

import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Database         ( DbAccess(..)
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
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import           Misc                           ( (?), fromJson )
import           Model.Posts                    ( PostFull )


data Sort = Sort SortBy Bool

data SortBy =
    ByDate
  | ByAuthor
  | ByCategory
  | ByNumberOfImages
  deriving (Read)

data TagsInAll = TagsInAll [Id] [Id]

data CreatedAt =
    CreatedAt Date
  | CreatedAtRange (Maybe Date) (Maybe Date)

search
  :: (DbAccess m, HasEnv Config m)
  => Page
  -> Sort
  -> TagsInAll
  -> CreatedAt
  -> Maybe Text
  -> Maybe Id
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
  let q = "SELECT count(*) OVER(), json FROM posts_view " <> mbjoin <> [sql|
           WHERE
               published = true
           AND COALESCE (authorname =      ?, true)
           AND COALESCE (categoryid =      ?, true)
           AND COALESCE (title ~~*         ?, true)
           AND COALESCE (content ~~*       ?, true)

           AND COALESCE (tags @>           ?, true)
           AND COALESCE (tags &&           ?, true)

           AND COALESCE (date =            ?, true)
           AND COALESCE (date >            ?, true)
           AND COALESCE (date <            ?, true)

           OR ( COALESCE (content ~~*      ?, false)
             OR COALESCE (title ~~*        ?, false)
             OR COALESCE (authorname ~~*   ?, false)
             OR COALESCE (categoryname ~~* ?, false)
             OR COALESCE (tags.tag ~~*     ?, false)
              )
           |]
           <> orderBy sortBy
           <> " LIMIT ? OFFSET ? "
  pageSize <- posts . page_sizes <$> getEnv
  mapM (fromJson . fromOnly) =<< queryPaged pageSize q
      ( mbAuthor
      , mbCategoryId
      , anywhere <$> mbTitle
      , anywhere <$> mbContent

      , PGArray tagsIn
      , PGArray tagsAll

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
  search = anywhere <$> mbEverywhere
  (dateEq, dateGt, dateLt) =
    case createdAt of
      CreatedAt date -> (Just date, Nothing, Nothing)
      CreatedAtRange from to -> (Nothing, from, to)
  anywhere text = "%" <> text <> "%"
  mbjoin = isNothing mbEverywhere ? "" $ [sql|
        LEFT JOIN tag_post_relations ON post = posts_view.id
        LEFT JOIN tags ON tags.id = tag_post_relations.tag
    |]

orderBy (Sort sort rev) = " ORDER BY " <> f sort <> if rev then " ASC " else " DESC "
  where
    f ByDate           = "date"
    f ByAuthor         = "authorname"
    f ByCategory       = "categoryname"
    f ByNumberOfImages = "numberofimages"

  {-

instance FromQuery CreatedAt where
    parseQuery q =
            CreatedAt <$> (readT =<< "created_at" .: q)
        <|> Just (CreatedAtRange
                (readT =<< "created_at__gt" .: q)
                (readT =<< "created_at__lt" .: q))

instance FromQuery TagsInAll where
  parseQuery q = Just $ TagsInAll
    (fromMaybe [] $ readT =<< ("tags__in" .: q))
    (let tags  = fromMaybe [] (readT =<< ("tags__all" .: q))
         mbtag = maybeToList (readT =<< "tag" .: q)
      in tags ++ mbtag)

instance FromQuery Sort where
  parseQuery q = Sort
    <$> (readT =<< param "sort")
    <*> Just (isJust (opt "sort_reversed"))

    -}
