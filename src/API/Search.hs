{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  #-}

module API.Search (posts) where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.Database         ( DbAccess(..)
                                                , Query(..)
                                                , limit
                                                , offset
                                                , queryPaged
                                                , sql
                                                )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Applicative            ( (<|>) )
import           Data.Maybe                     ( isNothing
                                                , isJust
                                                , fromMaybe
                                                , maybeToList
                                                )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..) )
import           Entities                       ( Date
                                                , AuthorName(..)
                                                , Content(..)
                                                , Search(..)
                                                , Title(..)
                                                , CategoryId(..)
                                                , Page(..)
                                                )
import           Misc                           ( (?)
                                                , readT
                                                )
import           FromQuery                      ( (.:)
                                                , (.:?)
                                                , FromQuery(..)
                                                )

searchPageSize = 20;

posts
  :: (HasLog m, DbAccess m, MonadError Text m)
  => ( Maybe Sort
     , TagsInAll
     , CreatedAt
     , Maybe AuthorName
     , Maybe CategoryId
     , Maybe Title
     , Maybe Content
     , Maybe Search
     , Page
     )
  -> m AppResponse

posts (mbSort, TagsInAll tags_in tags_all, createdAt, mbAuthor, mbcid, mbtitle, mbcontent, mbsearch, Page page) = do
    let q = "SELECT count(*) OVER(), json FROM posts_view " <> mbjoin <> [sql|
                 WHERE
                 published = true
             AND authorname = ?
             AND categoryid = ?
             AND title ~~* ?
             AND content ~~* ?

             AND tags @> ?
             AND tags && ?

             AND date = ?
             AND date > ?
             AND date < ?

             OR ( content ~~* ?
               OR title ~~* ?
               OR authorname ~~* ?
               OR categoryname ~~* ?
               OR tags.tag ~~* ?
                )
            |]
            <> orderBy mbSort
            <> " LIMIT ? OFFSET ? "
    log' Debug $ "build query: " <> decodeUtf8 (fromQuery q)
    queryPaged searchPageSize q
      ( unAuthorName <$> mbAuthor
      , mbcid
      , anywhere . unTitle <$> mbtitle
      , anywhere . unContent <$> mbcontent

      , PGArray tags_in
      , PGArray tags_all

      , dateEq
      , dateGt
      , dateLt

      , anywhere . unSearch <$> mbsearch
      , anywhere . unSearch <$> mbsearch
      , anywhere . unSearch <$> mbsearch
      , anywhere . unSearch <$> mbsearch
      , anywhere . unSearch <$> mbsearch

      , limit searchPageSize
      , offset searchPageSize page
      )
 where
  (dateEq, dateGt, dateLt) =
    case createdAt of
      CreatedAt date -> (Just date, Nothing, Nothing)
      CreatedAtRange from to -> (Nothing, from, to)
  anywhere text = "%" <> text <> "%"
  mbjoin = isNothing mbsearch ? "" $ [sql|
        LEFT JOIN tag_post_relations ON post = posts_view.id
        LEFT JOIN tags ON tags.id = tag_post_relations.tag
    |]

  {-
    posts (mbSort, TagsInAll tags_in tags_all, createdAt, mbAuthor, mbcid, mbtitle, mbcontent, mbsearch, Page page) (log, db)
  = catchDb log (return InternalError) $ do
    conditions <- sequence conditions
    searchCond <- sequence searchCond
    let q = Query $
               "SELECT count(*) OVER(), json FROM posts_view "
            <> fromQuery mbjoin
            <> " WHERE "
            <> intercalate " AND " ("published = true" : conditions)
            <> " AND ("
            <> (null searchCond ? "true" $ intercalate " OR " searchCond)
            <> ") " <> orderBy mbSort
            <> " LIMIT ? OFFSET ? "
    _ <- log Debug $ decodeUtf8 $ fromQuery q
    q <- query db q (limit searchPageSize, offset searchPageSize page) :: IO [(Int, J.Value)]
    return $ paginate searchPageSize q
 where
  conditions =
        (null tags_in  ? [] $ [formatQuery "tags @> ?" [PGArray tags_in]])
     ++ (null tags_all ? [] $ [formatQuery "tags && ?" [PGArray tags_all]])
     ++ (case createdAt of
          CreatedAt date -> [formatQuery "date = ?" [date]]
          CreatedAtRange Nothing Nothing -> []
          CreatedAtRange from to -> maybe [] (\from -> [formatQuery "date > ?" [from]]) from
                                 ++ maybe [] (\to -> [formatQuery "date < ?" [to]]) to)
     ++ maybe [] (\(AuthorName author) -> [formatQuery "authorname = ?" [author]]) mbAuthor
     ++ maybe [] (\(CategoryId cid) ->    [formatQuery "categoryid = ?" [cid]]) mbcid
     ++ maybe [] (\(Title title) ->       [formatQuery "title ~~* ?" [anywhere title]]) mbtitle
     ++ maybe [] (\(Content text) ->      [formatQuery "content ~~* ?" [anywhere text]]) mbcontent
  searchCond = maybe [] (\(Search text) ->
    [ formatQuery "content ~~* ?"      [anywhere text]
    , formatQuery "title ~~* ?"        [anywhere text]
    , formatQuery "authorname ~~* ?"   [anywhere text]
    , formatQuery "categoryname ~~* ?" [anywhere text]
    , formatQuery "tags.tag ~~* ?"     [anywhere text]
    ]) mbsearch
  anywhere text = "%" <> text <> "%"
  mbjoin = isNothing mbsearch ? "" $ [sql|
        LEFT JOIN tag_post_relations ON post = posts_view.id
        LEFT JOIN tags ON tags.id = tag_post_relations.tag
    |]
  -}

orderBy mbSort = " ORDER BY " <> case fromMaybe (Sort ByDate True) mbSort of
    Sort ByDate r           -> "date"           <> sort r
    Sort ByAuthor r         -> "authorname"     <> sort r
    Sort ByCategory r       -> "categoryname"   <> sort r
    Sort ByNumberOfImages r -> "numberofimages" <> sort r
  where sort r = if r then " ASC " else " DESC "

data CreatedAt =
    CreatedAt Date
  | CreatedAtRange (Maybe Date) (Maybe Date)

instance FromQuery CreatedAt where
    parseQuery q =
            CreatedAt <$> (readT =<< "created_at" .: q)
        <|> Just (CreatedAtRange
                (readT =<< "created_at__gt" .: q)
                (readT =<< "created_at__lt" .: q))

data TagsInAll = TagsInAll [Int] [Int]

instance FromQuery TagsInAll where
  parseQuery q = Just $ TagsInAll
    (fromMaybe [] $ readT =<< ("tags__in" .: q))
    (let tags  = fromMaybe [] (readT =<< ("tags__all" .: q))
         mbtag = maybeToList (readT =<< "tag" .: q)
      in tags ++ mbtag)

data Sort = Sort SortBy Bool

data SortBy =
    ByDate
  | ByAuthor
  | ByCategory
  | ByNumberOfImages
  deriving (Read)

instance FromQuery Sort where
  parseQuery q = Sort
    <$> (readT =<< "sort" .: q)
    <*> Just (isJust ("sort_reversed" .:? q))
