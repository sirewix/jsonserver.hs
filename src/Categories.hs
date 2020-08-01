{-# LANGUAGE OverloadedStrings #-}

module Categories where
import           App
import           Data.Aeson                     ( (.=) )
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple     ( query
                                                , execute
                                                , Only(..)
                                                )
import           Entities
import           Logger
import           Misc
import           Query
import qualified Data.Aeson                    as J

get_categories :: Maybe CategoryId -> Endpoint
get_categories mbcid (log, db) = catchDb log (return BadRequest) $ do
  let cond = case mbcid of
        Just a  -> "parent_id = ?"
        Nothing -> "parent_id is ?"
  q <- query db ("SELECT id, name FROM categories WHERE " <> cond) [mbcid] :: IO [Category]
  return . AppOk $ J.toJSON q

create_category :: UserName -> (Name, Maybe CategoryId) -> Endpoint
create_category (UserName admin) (Name category, mbcid) = queryOne
  "INSERT INTO categories (name, parent_id) VALUES (?, ?) RETURNING id"
  (category, mbcid)
  (J.Number . fromInteger)
  (Just $ \q -> admin <> " created category " <> showText q <> " '" <> category <> "'")

edit_category (UserName admin) (Name category, CategoryId cid) = execdb
  "UPDATE categories SET category = ? WHERE id = ?"
  (category, cid)
  (Just $ admin <> " changed category " <> showText cid <> " to '" <> category <> "'")

delete_category (UserName admin) (CategoryId cid) =
  execdb "DELETE FROM categories WHERE id = ?" [cid] (Just $ admin <> " deleted category " <> showText cid)
