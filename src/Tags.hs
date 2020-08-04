{-# LANGUAGE
    OverloadedStrings
  , QuasiQuotes
  #-}

module Tags where

import           App
import           Database.PostgreSQL.Simple.SqlQQ
import           Entities
import           Misc
import qualified Data.Aeson                    as J

tagsPageSize = 50

getTags (Page page) = queryPaged
  tagsPageSize
  [sql|
    SELECT
        count(*) OVER(),
        to_json(tags)
    FROM tags LIMIT ? OFFSET ?
  |]
  (limit tagsPageSize, offset tagsPageSize page)

createTag (UserName admin) (Name tag) = queryOne
  "INSERT INTO tags (tag) VALUES (?) RETURNING id"
  [tag]
  (J.Number . fromInteger)
  (Just $ \q -> admin <> " created tag " <> showText q <> " '" <> tag <> "'")

editTag (UserName admin) (Tag tid, Name newtag) = execdb
  "UPDATE tags SET tag = ? WHERE id = ?"
  (newtag, tid)
  (Just $ admin <> " changed tag " <> showText tid <> " to '" <> newtag <> "'")

deleteTag (UserName admin) (Tag tid) = execdb
  "DELETE FROM tags WHERE id = ?"
  [tid]
  (Just $ admin <> " deleted tag " <> showText tid)
