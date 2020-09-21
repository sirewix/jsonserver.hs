{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  #-}

module API.Authors where

import           App.Prototype.Database         ( execOne
                                                , queryPaged
                                                , limit
                                                , offset
                                                , sql
                                                )
import           Entities                       ( UserName(..)
                                                , Page(..)
                                                , Description(..)
                                                )

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

makeAuthor (UserName admin) (UserName name, Description description) = execOne
  "INSERT INTO authors (username, description) VALUES (?, ?)"
  (name, description)
  (Just $ admin <> " promoted " <> name <> " to authors with description \"" <> description <> "\"")

editAuthor (UserName admin) (UserName name, Description description) = execOne
  "UPDATE authors SET description = ? WHERE username = ?"
  (description, name)
  (Just $ admin <> " edited " <> name <> "'s description to \"" <> description <> "\"")

deleteAuthor (UserName admin) (UserName name) = execOne
  "DELETE FROM authors WHERE username = ?"
  [name]
  (Just $ admin <> " exiled " <> name <> " from authors guild")
