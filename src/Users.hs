{-# LANGUAGE
  OverloadedStrings
, QuasiQuotes
#-}

module Users where
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
import           Query
import           Auth
import qualified Data.Aeson                    as J
import           Database.PostgreSQL.Simple.SqlQQ

usersPageSize = 20

get_users (Page page) (log, db) =
  catchDb log (return BadRequest) $ do
    q <- query db [sql|
        SELECT
            count(*) OVER(),
            json_build_object (
                'name', name,
                'last_name', lastName,
                'admin', admin,
                'avatar', avatar,
                'registration_date', registrationDate
            )
        FROM users
        LIMIT ?
        OFFSET ?
      |] (limit usersPageSize, offset usersPageSize page) :: IO [(Int, J.Value)]
    return $ paginate usersPageSize q

create_user (UserName admin) = register -- eew

delete_user (UserName admin) (UserName user) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "DELETE FROM users WHERE name = ?" [user]
    if dbres == 1
      then do
        log Info $ admin <> " deleted user " <> user
        return . AppOk $ J.Null
      else return BadRequest
