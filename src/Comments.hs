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

commentsPageSize = 20

get_comments (PostId pid) (Page page) (log, db) =
  catchDb log (return BadRequest) $ do
    print "hi"
    q <- query db [sql|
        SELECT
            count(*) OVER(),
            json_build_object (
                'user', json_build_object(
                    'name', name,
                    'last_name', lastName,
                    'admin', admin,
                    'avatar', avatar,
                    'registration_date', registrationDate
                ),
                'comment', comment
            )
        FROM comments
        JOIN users ON name = username
        WHERE post = ?
        LIMIT ?
        OFFSET ?
      |] (pid, limit commentsPageSize, offset commentsPageSize page) :: IO [(Int, J.Value)]
    return $ paginate commentsPageSize q
