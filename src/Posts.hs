{-# LANGUAGE
    OverloadedStrings
  , QuasiQuotes
  #-}

module Posts where

import           App                            ( Endpoint
                                                , execdb
                                                , limit
                                                , offset
                                                , queryOne
                                                , queryPaged
                                                )
import           Database.PostgreSQL.Simple.SqlQQ(sql)
import           Entities                       ( UserName(..)
                                                , Content(..)
                                                , Title(..)
                                                , CategoryId(..)
                                                , Page(..)
                                                , PostId(..)
                                                , Image(..)
                                                , Images(..)
                                                , Tag(..)
                                                )
import           Misc                           ( showText )
import qualified Data.Aeson                    as J

postsPageSize = 20

post (PostId pid) () = queryOne
  "SELECT json FROM posts_view WHERE id = ? AND published = true"
  [pid]
  id
  Nothing

getPost (UserName author) (PostId pid) = queryOne
  "SELECT json FROM posts_view WHERE id = ? AND authorname = ?"
  (pid, author)
  id
  Nothing

getPosts (UserName author) (Page page) = queryPaged
  postsPageSize
  [sql|
    SELECT
        count(*) OVER(),
        json
    FROM posts_view
    WHERE authorname = ?
    LIMIT ?
    OFFSET ?
  |]
  (author, limit postsPageSize, offset postsPageSize page)

createPost (UserName author) (Title title, CategoryId cid, Content content, Image img, Images images)
  = queryOne
    [sql|
    INSERT INTO posts (title, date, author, category, content, mainImage, images, published)
    VALUES (?, current_timestamp, (SELECT id FROM authors WHERE username = ?), ?, ?, ?, ?, false) RETURNING id
    |]
    (title, author, cid, content, img, Images images)
    (J.Number . fromInteger)
    (Just $ \q -> author <> " created post " <> showText q <> " titled '" <> title <> "'")

attachTag (UserName author) (PostId pid, Tag tag) = execdb
  [sql|
    INSERT INTO tag_post_relations (tag, post)
    (SELECT (?), id FROM posts WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?))
  |]
  (tag, pid, author)
  Nothing

deattachTag (UserName author) (PostId pid, Tag tag) = execdb
  [sql|
    DELETE FROM tag_post_relations WHERE tag = ? AND post =
    (SELECT id FROM posts WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?))
  |]
  (tag, pid, author)
  Nothing

editPost
  :: UserName
  -> ( PostId
     , Maybe Title
     , Maybe CategoryId
     , Maybe Content
     , Maybe Image
     , Maybe Images
     )
  -> Endpoint
editPost (UserName author) (PostId pid, mbtitle, mbcategory, mbcontent, mbimg, mbimgs)
  = execdb
    [sql|
    UPDATE posts SET
    title = COALESCE (?, title),
    category = COALESCE (?, category),
    content = COALESCE (?, content),
    mainImage = COALESCE (?, mainImage),
    images = COALESCE (?, images),
    date = current_timestamp
    WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?)
    |]
    (mbtitle, mbcategory, mbcontent, mbimg, mbimgs, pid, author)
    (Just $ author <> " changed post " <> showText pid)

publishPost (UserName author) (PostId pid) = execdb
  [sql|
    UPDATE posts
    SET published = true
    WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?)
  |]
  (pid, author)
  (Just $ author <> " published post " <> showText pid)

deletePost (UserName author) (PostId pid) = execdb
  [sql|
    DELETE FROM posts
    WHERE id = ? AND author = (SELECT id FROM authors WHERE username = ?)
  |]
  (pid, author)
  (Just $ author <> " deleted post " <> showText pid)
