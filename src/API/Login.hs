{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}
module API.Login where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.Database         ( DbAccess(..)
                                                , Only(..)
                                                , execOne
                                                )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
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
  q <- query "SELECT admin FROM users WHERE name = ? AND password = ?" (name, password)
  case q of
    [Only admin] -> do
      token <- genToken (admin, False, name)
      log' Info $ name <> " logged in"
      return . AppOk $ J.String token
    _ -> return AccessDenied

