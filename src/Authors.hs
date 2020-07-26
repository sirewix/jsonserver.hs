{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
#-}
module Authors
    ( make_author
    , get_authors
    , edit_author
    , delete_author
    ) where

import           App
import           Misc
import           Entities
import           Database.PostgreSQL.Simple
import           Data.String (fromString)
import qualified Data.Aeson                    as J
import           Control.Exception
import           Logger

get_authors (UserName admin) () (log, db) =
  catchDb log (return InternalError) $ do
    q <- query db "SELECT id, username, description FROM authors" () :: IO [Author]
    return . AppOk $ J.toJSON q

make_author (UserName admin) (UserName name, Description description) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "INSERT INTO authors (username, description) VALUES (?, ?)" (name, description)
    if dbres == 1
      then do
        log Info $ admin <> " promoted " <> name <> " to authors with description \"" <> description <> "\""
        return . AppOk $ J.Null
      else return BadRequest

edit_author (UserName admin) (UserName name, Description description) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "UPDATE authors SET description = ? WHERE username = ?" (description, name)
    if dbres == 1
      then do
        log Info $ admin <> " edited " <> name <> "'s description to \"" <> description <> "\""
        return . AppOk $ J.Null
      else return BadRequest

delete_author (UserName admin) (UserName name) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "DELETE FROM authors WHERE username = ?" [name]
    if dbres == 1
      then do
        log Info $ admin <> " exiled " <> name <> " from authors guild"
        return . AppOk $ J.Null
      else return BadRequest
