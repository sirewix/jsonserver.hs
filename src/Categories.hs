{-# LANGUAGE OverloadedStrings #-}

module Categories where
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
import qualified Data.Aeson                    as J
import           Data.Aeson                     ( (.=) )

get_categories :: Maybe CategoryId -> Endpoint
get_categories mbcid (log, db) =
  catchDb log (return BadRequest) $ do
    let cond = case mbcid of
         Just a -> "parent_id = ?"
         Nothing -> "parent_id is ?"
    q <- query db ("SELECT id, name FROM categories WHERE " <> cond) [mbcid] :: IO [Category]
    return . AppOk $ J.toJSON q

create_category :: UserName -> (Name, Maybe CategoryId) -> Endpoint
create_category (UserName admin) (Name category, mbcid) (log, db) =
  catchDb log (return BadRequest) $ do
    [Only cid] <- query db "INSERT INTO categories (name, parent_id) VALUES (?, ?) RETURNING id" (category, mbcid)
    log Info $ admin <> " created category '" <> category <> "'"
    return . AppOk $ J.Number $ fromInteger cid

edit_category (UserName admin) (Name category, CategoryId cid) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "UPDATE categories SET category = ? WHERE id = ?" (category, cid)
    if dbres == 1
      then do
        log Info $ admin <> " changed category " <> showText cid <> " to '" <> category <> "'"
        return . AppOk $ J.Null
      else return BadRequest

delete_category (UserName admin) (CategoryId cid) (log, db) =
  catchDb log (return BadRequest) $ do
    dbres <- execute db "DELETE FROM categories WHERE id = ?" [cid]
    if dbres == 1
      then do
        log Info $ admin <> " deleted category " <> showText cid
        return . AppOk $ J.Null
      else return BadRequest
