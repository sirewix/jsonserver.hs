{-# LANGUAGE
    OverloadedStrings
  , QuasiQuotes
  #-}

module Comments where
import           App
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple.SqlQQ
import           Entities
import           Misc
import           Query
import qualified Data.Aeson                    as J

commentsPageSize = 20

getComments (PostId pid) (Page page) = queryPaged
  commentsPageSize
  [sql|
    SELECT
        count(*) OVER(),
        json_build_object (
            'id', id,
            'comment', comment,
            'user', json_build_object(
                'name', name,
                'lastname', lastName,
                'admin', admin,
                'avatar', avatar,
                'registration_date', registrationDate
            )
        )
    FROM comments
    JOIN users ON name = username
    WHERE post = ?
    LIMIT ?
    OFFSET ?
  |]
  (pid, limit commentsPageSize, offset commentsPageSize page)

addComment (UserName user) (PostId pid, Content text) = queryOne
  "INSERT INTO comments (post, username, comment) VALUES (?, ?, ?) RETURNING id"
  (pid, user, text)
  (J.Number . fromInteger)
  (Just $ \q -> user <> " added comment " <> showText q <> " to post " <> showText pid)

newtype CommentId = CommentId Text
instance Query CommentId where
  parseQuery q = CommentId <$> "cid" .: q

deleteComment (UserName user) (CommentId cid) = execdb
  "DELETE FROM comments WHERE id = ?"
  [cid]
  (Just $ user <> " deleted comment " <> showText cid)
