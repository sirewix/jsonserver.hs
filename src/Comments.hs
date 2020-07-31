{-# LANGUAGE
  OverloadedStrings
, QuasiQuotes
#-}
module Comments where
import           App
import           Entities
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple     ( query
                                                , execute
                                                , Only(..)
                                                )
import qualified Data.Aeson                    as J
import           Logger
import           Misc
import           Query
import           Data.Text                      ( Text )

commentsPageSize = 20

get_comments (PostId pid) (Page page) (log, db) =
  catchDb log (return BadRequest) $ do
    q <- query db [sql|
        SELECT
            count(*) OVER(),
            json_build_object (
                'id', id,
                'comment', comment,
                'user', json_build_object(
                    'name', name,
                    'last_name', lastName,
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
      |] (pid, limit commentsPageSize, offset commentsPageSize page) :: IO [(Int, J.Value)]
    return $ paginate commentsPageSize q

add_comment (UserName user) (PostId pid, Content text) (log, db) =
  catchDb log (return BadRequest) $ do
    [Only q] <- query db "INSERT INTO comments (post, username, comment) VALUES (?, ?, ?) RETURNING id" (pid, user, text) :: IO [Only Integer]
    log Info $ user <> " added comment " <> showText q <> " to post " <> showText pid
    return . AppOk $ J.Number $ fromInteger q

newtype CommentId = CommentId Text
instance Query CommentId where
  parseQuery q = CommentId <$> "cid" .: q


delete_comment (UserName user) (CommentId cid) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "DELETE FROM comments WHERE id = ?" [cid]
    if dbres == 1
      then do
        log Info $ user <> " deleted comment " <> showText cid
        return . AppOk $ J.Null
      else return BadRequest
