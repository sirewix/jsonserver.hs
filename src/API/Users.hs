{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  #-}

module API.Users where

import           API.Login                      ( register )
import           App.Prototype.Database         ( execOne
                                                , limit
                                                , offset
                                                , queryPaged
                                                , sql
                                                )
import           Entities                       ( Page(..)
                                                , UserName(..)
                                                )

usersPageSize = 20

getUsers (Page page) = queryPaged
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

createUser (UserName _admin) = register -- eew

deleteUser (UserName admin) (UserName user) = execOne
  "DELETE FROM users WHERE name = ?"
  [user]
  (Just $ admin <> " deleted user " <> user)
