{-# LANGUAGE
  OverloadedStrings
, QuasiQuotes
#-}

module Authors where

import           App
import           Database.PostgreSQL.Simple.SqlQQ
import           Entities
import           Misc

authorsPageSize = 20

get_authors (UserName admin) (Page page) = queryPaged
  authorsPageSize
  [sql|
    SELECT
        count(*) OVER(),
        to_json (authors)
    FROM authors
    LIMIT ?
    OFFSET ?
  |]
  (limit authorsPageSize, offset authorsPageSize page)

make_author (UserName admin) (UserName name, Description description) = execdb
  "INSERT INTO authors (username, description) VALUES (?, ?)"
  (name, description)
  (Just $ admin <> " promoted " <> name <> " to authors with description \"" <> description <> "\"")

edit_author (UserName admin) (UserName name, Description description) = execdb
  "UPDATE authors SET description = ? WHERE username = ?"
  (description, name)
  (Just $ admin <> " edited " <> name <> "'s description to \"" <> description <> "\"")

delete_author (UserName admin) (UserName name) = execdb
  "DELETE FROM authors WHERE username = ?"
  [name]
  (Just $ admin <> " exiled " <> name <> " from authors guild")
