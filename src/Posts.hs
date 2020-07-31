{-# LANGUAGE
  OverloadedStrings
, QuasiQuotes
#-}

module Posts where

import           App
import           Misc
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple     ( query
                                                , execute
                                                , Only(..)
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
import           Entities
import           Logger
import qualified Data.Aeson                    as J

postsPageSize = 20

get_post (UserName author) (PostId pid) (log, db) =
  catchDb log (return BadRequest) $ do
    [Only p] <- query db [sql|
        SELECT json FROM posts_view WHERE id = ? AND authorname = ?
      |] (pid, author) :: IO [Only J.Value]
    return . AppOk $ p

get_posts (UserName author) (Page page) (log, db) =
  catchDb log (return InternalError) $ do
    q <- query db [sql|
        SELECT count(*) OVER(), json
            FROM posts_view
            WHERE authorname = ?
            LIMIT ?
            OFFSET ?
      |] (author, limit postsPageSize, offset postsPageSize page) :: IO [(Int, J.Value)]
    return $ paginate postsPageSize q

create_post (UserName author) (Title title, CategoryId cid, Content content, Image img, Images images) (log, db) =
  catchDb log (return BadRequest) $ do
    [Only pid] <- query db [sql|
        INSERT INTO posts (title, date, author, category, content, mainImage, images, published)
        VALUES (?, current_timestamp, (SELECT id FROM authors WHERE username = ?), ?, ?, ?, ?, false) RETURNING id;
        REFRESH MATERIALIZED VIEW posts_view;
      |] (title, author, cid, content, img, Images images)
    log Info $ author <> " created post '" <> title <> "'"
    return . AppOk $ J.Number $ fromInteger pid

attach_tag (UserName author) (PostId pid, Tag tag) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db [sql|
        INSERT INTO tag_post_relations (tag, post)
        (SELECT (?), id FROM posts WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?));
        REFRESH MATERIALIZED VIEW posts_view;
     |] (tag, pid, author)
    if dbres == 1
      then return $ AppOk J.Null
      else return BadRequest

deattach_tag (UserName author) (PostId pid, Tag tag) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db [sql|
        DELETE FROM tag_post_relations WHERE tag = ? AND post =
        (SELECT id FROM posts WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?));
        REFRESH MATERIALIZED VIEW posts_view;
     |] (tag, pid, author)
    if dbres == 1
      then return $ AppOk J.Null
      else return BadRequest

edit_post :: UserName -> (PostId, Maybe Title, Maybe CategoryId, Maybe Content, Maybe Image, Maybe Images) -> Endpoint
edit_post (UserName author) (PostId pid, mbtitle, mbcategory, mbcontent, mbimg, mbimgs) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db [sql|
        UPDATE posts SET
        title = COALESCE (?, title),
        category = COALESCE (?, category),
        content = COALESCE (?, content),
        mainImage = COALESCE (?, mainImage),
        images = COALESCE (?, images),
        date = current_timestamp
        WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?);
        REFRESH MATERIALIZED VIEW posts_view;
     |] (mbtitle, mbcategory, mbcontent, mbimg, mbimgs, pid, author)
    if dbres == 1
      then do
        log Info $ author <> " changed post " <> showText pid
        return . AppOk $ J.Null
      else return BadRequest

publish_post (UserName author) (PostId pid) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db [sql|
        UPDATE posts
        SET published = true
        WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?);
        REFRESH MATERIALIZED VIEW posts_view;
     |] (pid, author)
    if dbres == 1
      then do
        log Info $ author <> " published post " <> showText pid
        return . AppOk $ J.Null
      else return BadRequest

delete_post (UserName author) (PostId pid) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db [sql|
        DELETE FROM posts
        WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?);
        REFRESH MATERIALIZED VIEW posts_view;
     |] (pid, author)
    if dbres == 1
      then do
        log Info $ author <> " deleted post " <> showText pid
        return . AppOk $ J.Null
      else return BadRequest
