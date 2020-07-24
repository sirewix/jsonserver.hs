{-# LANGUAGE OverloadedStrings #-}

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

get_users () (log, db) =
  catchDb log (return BadRequest) $ do
    q <- query db "SELECT name, lastName, admin, avatar, registrationDate FROM users" () :: IO [User]
    return . AppOk $ J.toJSON q

create_user (UserName admin) = register

delete_user (UserName admin) (UserName user) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "DELETE FROM users WHERE name = ?" [user]
    if dbres == 1
      then do
        log Info $ admin <> " deleted user " <> user
        return . AppOk $ J.Null
      else return BadRequest
