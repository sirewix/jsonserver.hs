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
import           Data.Aeson ((.=))
import           Database.PostgreSQL.Simple.Time
import           Control.Applicative
import           Data.Maybe
import           Data.ByteString(ByteString, intercalate)
import qualified Data.ByteString.Char8         as B
import           Database.PostgreSQL.Simple.Types (PGArray(..))

searchPageSize = 20;

posts :: ( Maybe Sort
         , TagsInAll
         , CreatedAt
         , Maybe AuthorName
         , Maybe CategoryId
         , Maybe Title
         , Maybe Content
         , Maybe Search
         , Page
         ) -> Endpoint

posts (mbSort, TagsInAll tags_in tags_all, createdAt, mbAuthor, mbcid, mbtitle, mbcontent, mbsearch, Page page) (log, db)
  = catchDb log (return InternalError) $ do
    conditions <- sequence conditions
    searchCond <- sequence searchCond
    let q = P.Query $
               "SELECT count(*) OVER(), json FROM posts_view "
            <> P.fromQuery mbjoin
            <> " WHERE "
            <> intercalate " AND " ("published = true" : conditions)
            <> " AND ("
            <> (null searchCond ? "true" $ intercalate " OR " searchCond)
            <> ") LIMIT ? OFFSET ? "
    log Debug $ decodeUtf8 $ P.fromQuery q
    q <- query db q (limit searchPageSize, offset searchPageSize page) :: IO [(Int, J.Value)]
    return $ paginate searchPageSize q
 where
  conditions =
        (null tags_in  ? [] $ [formatQuery db "tags @> ?" [PGArray tags_in]])
     ++ (null tags_all ? [] $ [formatQuery db "tags && ?" [PGArray tags_all]])
     ++ (case createdAt of
          CreatedAt date -> [formatQuery db "date = ?" [date]]
          CreatedAtRange Nothing Nothing -> []
          CreatedAtRange from to -> maybe [] (\from -> [formatQuery db "date > ?" [from]]) from
                                 ++ maybe [] (\to -> [formatQuery db "date < ?" [to]]) to)
     ++ maybe [] (\(AuthorName author) -> [formatQuery db "authorname = ?" [author]]) mbAuthor
     ++ maybe [] (\(CategoryId cid) ->    [formatQuery db "categoryid = ?" [cid]]) mbcid
     ++ maybe [] (\(Title title) ->       [formatQuery db "title ~~* ?" [anywhere title]]) mbtitle
     ++ maybe [] (\(Content text) ->      [formatQuery db "content ~~* ?" [anywhere text]]) mbcontent
  searchCond = maybe [] (\(Search text) ->
    [ formatQuery db "content ~~* ?"      [anywhere text]
    , formatQuery db "title ~~* ?"        [anywhere text]
    , formatQuery db "authorname ~~* ?"   [anywhere text]
    , formatQuery db "categoryname ~~* ?" [anywhere text]
    , formatQuery db "tags.tag ~~* ?"     [anywhere text]
    ]) mbsearch
  anywhere text = "%" <> text <> "%"
  mbjoin = isNothing mbsearch ? "" $ [sql|
        LEFT JOIN tag_post_relations ON post = posts_view.id
        LEFT JOIN tags ON tags.id = tag_post_relations.tag
    |]

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
    (let tags  = (maybe [] id $ readT =<< ("tags__all" .: q))
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
