{-# LANGUAGE
  OverloadedStrings
, QuasiQuotes
#-}

module Tags where

import           App
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple     ( query
                                                , execute
                                                , Only(..)
                                                )
import           Entities
import           Logger
import           Query
import           Misc
import qualified Data.Aeson                    as J
import           Data.Aeson                     ( (.=) )
import           Data.Yaml (array)
import           Database.PostgreSQL.Simple.SqlQQ

tagsPageSize = 50;

get_tags (Page page) (log, db) =
  catchDb log (return InternalError) $ do
    q <- query db [sql|
            SELECT
                count(*) OVER(),
                to_json(tags)
            FROM tags LIMIT ? OFFSET ?
        |] (limit tagsPageSize, offset tagsPageSize page) :: IO [(Int, J.Value)]
    return $ paginate tagsPageSize q

create_tag (UserName admin) (Name tag) (log, db) =
  catchDb log (return BadRequest) $ do
    [Only q] <- query db "INSERT INTO tags (tag) VALUES (?) RETURNING id" [tag] :: IO [Only Integer]
    log Info $ admin <> " created tag " <> showText q <> " '" <> tag <> "'"
    return . AppOk $ J.Number $ fromInteger q

edit_tag (UserName admin) (Tag tid, Name newtag) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "UPDATE tags SET tag = ? WHERE id = ?" (newtag, tid)
    if dbres == 1
      then do
        log Info $ admin <> " changed tag " <> showText tid <> " to '" <> newtag <> "'"
        return . AppOk $ J.Null
      else return BadRequest

delete_tag (UserName admin) (Tag tid) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "DELETE FROM tags WHERE id = ?" [tid]
    if dbres == 1
      then do
        log Info $ admin <> " deleted tag " <> showText tid
        return . AppOk $ J.Null
      else return BadRequest
