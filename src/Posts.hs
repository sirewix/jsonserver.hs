{-# LANGUAGE OverloadedStrings #-}

module Posts where

import           App
import           Misc
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple     ( query
                                                , execute
                                                , Only(..)
                                                )
import           Entities
import           Logger
import qualified Data.Aeson                    as J

get_posts () (log, db) = undefined
    {-
  catchDb log (return InternalError) $ do
    q <- query db "SELECT post FROM posts" () :: IO [Tag]
    return . AppOk $ J.toJSON q
    -}

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
