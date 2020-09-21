{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  #-}

module API.Comments where
import           App.Prototype.Database         ( execOne
                                                , limit
                                                , offset
                                                , queryOne
                                                , queryPaged
                                                , sql
                                                )
import           Data.Text                      ( Text )
import           Entities                       ( Page(..)
                                                , PostId(..)
                                                , UserName(..)
                                                , Content(..)
                                                )
import           Misc                           ( showText )
import           FromQuery                      ( (.:)
                                                , FromQuery(..)
                                                )
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
instance FromQuery CommentId where
  parseQuery q = CommentId <$> "cid" .: q

deleteComment (UserName user) (CommentId cid) = execOne
  "DELETE FROM comments WHERE id = ?"
  [cid]
  (Just $ user <> " deleted comment " <> showText cid)
