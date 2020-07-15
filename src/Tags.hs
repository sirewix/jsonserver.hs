{-# LANGUAGE OverloadedStrings #-}

module Tags where

import           App
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple     ( query
                                                , execute
                                                )
import           Entities
import           Logger
import           Query
import qualified Data.Aeson                    as J

get_tags () (log, db) =
  catchDb log (return InternalError) $ do
    q <- query db "SELECT tag FROM tags" () :: IO [Tag]
    return . AppOk $ J.toJSON q

create_tag (UserName admin) (Tag tag) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "INSERT INTO tags (tag) VALUES (?)" [tag]
    if dbres == 1
      then do
        log Info $ admin <> " created tag '" <> tag <> "'"
        return . AppOk $ J.Null
      else return BadRequest

instance Query EditTagOptions where
  parseQuery q = EditTagOptions
    <$> (decodeUtf8 <$> "tag" .: q)
    <*> (decodeUtf8 <$> "new_tag_name" .: q)

data EditTagOptions = EditTagOptions Text Text

edit_tag (UserName admin) (EditTagOptions tag newtag) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "UPDATE tags SET tag = ? WHERE tag = ?" (newtag, tag)
    if dbres == 1
      then do
        log Info $ admin <> " changed tag '" <> tag <> "' to '" <> newtag <> "'"
        return . AppOk $ J.Null
      else return BadRequest

delete_tag (UserName admin) (Tag tag) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "DELETE FROM tags WHERE tag = ?" [tag]
    if dbres == 1
      then do
        log Info $ admin <> " deleted tag '" <> tag <> "'"
        return . AppOk $ J.Null
      else return BadRequest
