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

get_post (UserName author) (PostId pid) (log, db) =
  catchDb log (return BadRequest) $ do
    [Only p] <- query db [sql|
      WITH a AS (SELECT id, username, description FROM authors WHERE username = ?),
           t AS (SELECT tag FROM tag_post_relations WHERE post = ?),
           p AS (SELECT posts.id, title, date, category, content, mainImage, images, published FROM posts
                 WHERE posts.id = ? AND author = (SELECT id FROM a)),
           c AS (WITH RECURSIVE x AS (
                    SELECT * FROM categories WHERE id = (SELECT category FROM p)
                    UNION ALL
                    SELECT categories.* from categories JOIN x ON x.parent_id = categories.id
                ) SELECT * FROM x)
      SELECT json_build_object (
           'id',            p.id,
           'title',         p.title,
           'date',          p.date,
           'content',       p.content,
           'mainImage',     p.mainImage,
           'images',        p.images,
           'published',     p.published,
           'author',        (SELECT json_agg(a) FROM a),
           'category_tree', (SELECT json_agg(c) FROM c),
           'tags',          array (SELECT tag FROM t)
      ) FROM p, a
      |] (author, pid, pid) :: IO [Only J.Value]
    return . AppOk $ p

create_post (UserName author) (Title title, CategoryId cid, Content content, Image img, Images images) (log, db) =
  catchDb log (return BadRequest) $ do
    [Only pid] <- query db
       ( "INSERT INTO posts (title, date, author, category, content, mainImage, images, published)"
      <> "VALUES (?, current_timestamp, (SELECT id FROM authors WHERE username = ?), ?, ?, ?, ?, false) RETURNING id"
       ) (title, author, cid, content, img, Images images)
    log Info $ author <> " created post '" <> title <> "'"
    return . AppOk $ J.Number $ fromInteger pid

attach_tag (UserName author) (PostId pid, Tag tag) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db
      ( "INSERT INTO tag_post_relations (tag, post) "
     <> "(SELECT (?), id FROM posts WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?))"
      ) (tag, pid, author)
    if dbres == 1
      then return $ AppOk J.Null
      else return BadRequest

deattach_tag (UserName author) (PostId pid, Tag tag) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db
      ( "DELETE FROM tag_post_relations WHERE tag = ? AND post = "
     <> "(SELECT id FROM posts WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?))"
      ) (tag, pid, author)
    if dbres == 1
      then return $ AppOk J.Null
      else return BadRequest

edit_post :: UserName -> (PostId, Maybe Title, Maybe CategoryId, Maybe Content, Maybe Image, Maybe Images) -> Endpoint
edit_post (UserName author) (PostId pid, mbtitle, mbcategory, mbcontent, mbimg, mbimgs) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db
      ( "UPDATE posts SET "
     <> "title = COALESCE (?, title),"
     <> "category = COALESCE (?, category),"
     <> "content = COALESCE (?, content),"
     <> "mainImage = COALESCE (?, mainImage),"
     <> "images = COALESCE (?, images),"
     <> "date = current_timestamp "
     <> "WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?)"
      ) (mbtitle, mbcategory, mbcontent, mbimg, mbimgs, pid, author)
    if dbres == 1
      then do
        log Info $ author <> " changed post " <> showText pid
        return . AppOk $ J.Null
      else return BadRequest

publish_post (UserName author) (PostId pid) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db
      ( "UPDATE posts SET "
     <> "published = true "
     <> "WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?)"
      ) (pid, author)
    if dbres == 1
      then do
        log Info $ author <> " published post " <> showText pid
        return . AppOk $ J.Null
      else return BadRequest

delete_post (UserName author) (PostId pid) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db
      ( "DELETE FROM posts "
     <> "WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?)"
      ) (pid, author)
    if dbres == 1
      then do
        log Info $ author <> " deleted post " <> showText pid
        return . AppOk $ J.Null
      else return BadRequest
