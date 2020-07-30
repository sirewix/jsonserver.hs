{-# LANGUAGE
  OverloadedStrings
, QuasiQuotes
#-}

module Search(posts, post) where
import           App
import           Query
import           Misc
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import qualified Database.PostgreSQL.Simple.Types as P
import           Database.PostgreSQL.Simple     ( query
                                                , execute
                                                , Only(..)
                                                , formatQuery
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
import           Entities
import           Logger
import qualified Data.Aeson                    as J
import           Data.Yaml (array)
import           Database.PostgreSQL.Simple.Time
import           Control.Applicative
import           Data.Maybe
import           Data.ByteString(ByteString, intercalate)
import qualified Data.ByteString.Char8         as B
import           Database.PostgreSQL.Simple.Types (PGArray(..))

data SELECT = SELECT [ByteString] FROM [JOIN] WHERE ORDER_BY GROUP_BY
data FROM = FROM [ByteString]
data WHERE = WHERE [ByteString]
data ORDER_BY = ORDER_BY [ByteString]
data GROUP_BY = GROUP_BY [ByteString]
data JOIN = JOIN_ON ByteString [ByteString]

class ToSQL q where
    toSQL :: q -> ByteString

instance ToSQL SELECT where
    toSQL (SELECT what from joins wher orderby groupby) =
        "SELECT " <> intercalate ", " what <> " " <> intercalate " "
            [ toSQL from
            , intercalate " " $ map toSQL joins
            , toSQL wher
            , toSQL orderby
            , toSQL groupby
            ]

instance ToSQL FROM where
    toSQL (FROM tables@(_:_)) = "FROM " <> intercalate ", " tables
    toSQL (FROM []) = ""

instance ToSQL WHERE where
    toSQL (WHERE conditions@(_:_)) = "WHERE " <> intercalate " AND " conditions
    toSQL (WHERE []) = ""

instance ToSQL ORDER_BY where
    toSQL (ORDER_BY columns@(_:_)) = "ORDER BY " <> intercalate ", " columns
    toSQL (ORDER_BY []) = ""

instance ToSQL GROUP_BY where
    toSQL (GROUP_BY columns@(_:_)) = "GROUP BY " <> intercalate ", " columns
    toSQL (GROUP_BY []) = ""

instance ToSQL JOIN where
    toSQL (JOIN_ON table conditions) = "JOIN " <> table <> " ON " <> intercalate ", " conditions

posts :: ( Maybe Sort
         , TagsInAll
         , CreatedAt
         , Maybe AuthorName
         , Maybe CategoryId
         ) -> Endpoint

posts (mbSort, TagsInAll tags_in tags_all, createdAt, mbAuthor, mbcid) (log, db)
  = catchDb log (return InternalError) $ do
    -- posts <- query db ( P.Query . toSQL $
    tagsCond <- sequence tagsCond
    createdCond <- sequence createdCond
    authorCond <- sequence authorCond
    categoryCond <- sequence categoryCond
    let q = P.Query . toSQL $ SELECT
          ["json"]
          (FROM ["posts_view"])
          []
          (WHERE $ ["published = true"]
                ++ tagsCond
                ++ createdCond
                ++ authorCond
                ++ categoryCond)
          (ORDER_BY [])
          (GROUP_BY [])
    posts <- query db q () :: IO [Only J.Value]
    return . AppOk . array $ map fromOnly posts
 where
  tagsCond = (null tags_in  ? [] $ [formatQuery db "tags @> ?" [PGArray tags_in]])
          ++ (null tags_all ? [] $ [formatQuery db "tags && ?" [PGArray tags_all]])
  createdCond = case createdAt of
      CreatedAt date -> [formatQuery db "date = ?" [date]]
      CreatedAtRange Nothing Nothing -> []
      CreatedAtRange from to -> maybe [] (\from -> [formatQuery db "date > ?" [from]]) from
                             ++ maybe [] (\to -> [formatQuery db "date < ?" [to]]) to
  authorCond = maybe [] (\(AuthorName author) -> [formatQuery db "authorname = ?" [author]]) mbAuthor
  categoryCond = maybe [] (\(CategoryId cid) -> [formatQuery db "categoryid = ?" [cid]]) mbcid

post (PostId pid) (log, db) =
  catchDb log (return BadRequest) $ do
    [Only post] <- query db "SELECT json FROM posts_view WHERE id = ? AND published = true" [pid]
    return . AppOk $ post

orderBy mbSort = " ORDER BY " <> case maybe (Sort ByDate True) id mbSort of
    Sort ByDate r           -> "date"           <> sort r
    Sort ByAuthor r         -> "authorname"     <> sort r
    Sort ByCategory r       -> "categoryname"   <> sort r
    Sort ByNumberOfImages r -> "numberofimages" <> sort r
  where sort r = if r then " ASC " else " DESC "

data CreatedAt =
    CreatedAt Date
  | CreatedAtRange (Maybe Date) (Maybe Date)

instance Query CreatedAt where
    parseQuery q =
            CreatedAt <$> (readT =<< "created_at" .: q)
        <|> (Just $ CreatedAtRange
                (readT =<< "created_at__gt" .: q)
                (readT =<< "created_at__lt" .: q))

data TagsInAll = TagsInAll [Int] [Int]

instance Query TagsInAll where
    parseQuery q = Just $ TagsInAll
          (maybe [] id $ readT =<< ("tags__in" .: q))
          (let tags = (maybe [] id $ readT =<< ("tags__all" .: q))
               mbtag = maybeToList (readT =<< "tag" .: q)
            in tags ++ mbtag)

data Sort = Sort SortBy Bool

data SortBy =
    ByDate
  | ByAuthor
  | ByCategory
  | ByNumberOfImages
  deriving (Read)

instance Query Sort where
  parseQuery q = Sort
    <$> (readT =<< "sort" .: q)
    <*> (Just $ maybe False (const True) ("sort_reversed" .:? q))

{-
API новостей должно поддерживать фильтрацию по полям:
    день создания (созданные ранее даты, созданные после даты, созданные в тот же день),
    имя автора,
    категория по айди,

    Примеры:
    /posts?created_at=2018-05-21
  | { /posts?created_at__lt=2018-05-21
    , /posts?created_at__gt=2018-05-21
    }

поиск по конкретному тегу
    тег по айди
    найти статьи, в которых есть хоть один тег из списка
    найти только те статьи, в которых есть все теги одновременно,

    Примеры:
    /posts?tag=123
  | { /posts?tags__in=[123,124,125]
    , /posts?tags__all=[123,124,125]
    }
название (вхождение подстроки)
контент (вхождение подстроки)
API новостей должно поддерживать поиск по строке, которая может быть найдена либо в текстовом контенте, либо в имени автора, либо в названии категории/тега
API новостей должно поддерживать сортировку по:
    дате,
    автору (имя по алфавиту),
    по категориям (название по алфавиту),
    по количеству фотографий
-}
