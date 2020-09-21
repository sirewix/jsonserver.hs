{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  , ScopedTypeVariables
  #-}
module API.Login where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.Database         ( DbAccess(..)
                                                , execOne
                                                , sql
                                                )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
import           Data.Maybe                     ( isJust )
import           Entities                       ( LastName(..)
                                                , UserName(..)
                                                , Password(..)
                                                )
import qualified Data.Aeson                    as J

register (UserName name, LastName lastName, Password password) = execOne
  "INSERT INTO users (name, lastname, registrationdate, admin, password) VALUES (?, ?, current_timestamp, false, ?)"
  (name, lastName, password)
  (Just $ "new user" <> name <> " " <> lastName)

login genToken (UserName name, Password password) = do
  q <- query [sql|
    SELECT
      (SELECT admin FROM users WHERE name = ? AND password = ?),
      (SELECT id FROM authors WHERE username = ?)
    |] (name, password, name)
  case q of
    [(Just isAdmin, isAuthor :: Maybe Int)] -> do
      token <- genToken (isAdmin, isJust isAuthor, name)
      log' Info $ name <> " logged in"
      return . AppOk $ J.String token
    _ -> return AccessDenied

