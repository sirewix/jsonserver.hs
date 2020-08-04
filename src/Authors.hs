{-# LANGUAGE
    OverloadedStrings
  , QuasiQuotes
  #-}

module Authors where

import           App
import           Database.PostgreSQL.Simple.SqlQQ
import           Entities

authorsPageSize = 20

getAuthors (UserName _admin) (Page page) = queryPaged
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

makeAuthor (UserName admin) (UserName name, Description description) = execdb
  "INSERT INTO authors (username, description) VALUES (?, ?)"
  (name, description)
  (Just $ admin <> " promoted " <> name <> " to authors with description \"" <> description <> "\"")

editAuthor (UserName admin) (UserName name, Description description) = execdb
  "UPDATE authors SET description = ? WHERE username = ?"
  (description, name)
  (Just $ admin <> " edited " <> name <> "'s description to \"" <> description <> "\"")

deleteAuthor (UserName admin) (UserName name) = execdb
  "DELETE FROM authors WHERE username = ?"
  [name]
  (Just $ admin <> " exiled " <> name <> " from authors guild")
