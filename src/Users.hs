{-# LANGUAGE
  OverloadedStrings
, QuasiQuotes
#-}

module Users where
import           App
import           Entities
import           Auth                           ( register )
import           Database.PostgreSQL.Simple.SqlQQ

usersPageSize = 20

get_users (Page page) = queryPaged
  usersPageSize
  [sql|
    SELECT
        count(*) OVER(),
        json_build_object (
            'name', name,
            'lastname', lastName,
            'admin', admin,
            'avatar', avatar,
            'registration_date', registrationDate
        )
    FROM users
    LIMIT ?
    OFFSET ?
  |]
  (limit usersPageSize, offset usersPageSize page)

create_user (UserName admin) = register -- eew

delete_user (UserName admin) (UserName user) = execdb
  "DELETE FROM users WHERE name = ?"
  [user]
  (Just $ admin <> " deleted user " <> user)
