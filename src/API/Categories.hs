{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  #-}

module API.Categories where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.Database         ( DbAccess(..)
                                                , Only(..)
                                                , execOne
                                                , queryOne
                                                , sql
                                                )
import           App.Prototype.Log              ( HasLog(..) )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Text                      ( Text )
import           Data.Yaml                      ( array )
import           Entities                       ( UserName(..)
                                                , CategoryId(..)
                                                , Name(..)
                                                )
import           Misc                           ( showText )
import qualified Data.Aeson                    as J

getCategories :: (Monad m, DbAccess m) => Maybe CategoryId -> m AppResponse
getCategories mbcid =
  let q = [sql|
            SELECT
              json_build_object (
                  'id', id,
                  'name', name,
              )
            FROM categories WHERE
          |]
      cond = case mbcid of
        Just _  -> "parent_id = ?"
        Nothing -> "parent_id is ?"
   in AppOk . array . map fromOnly <$> query (q <> cond) [mbcid]

createCategory
  :: (HasLog m, DbAccess m, MonadError Text m)
  => UserName
  -> (Name, Maybe CategoryId)
  -> m AppResponse
createCategory (UserName admin) (Name category, mbcid) = queryOne
  "INSERT INTO categories (name, parent_id) VALUES (?, ?) RETURNING id"
  (category, mbcid)
  (J.Number . fromInteger)
  (Just $ \q -> admin <> " created category " <> showText q <> " '" <> category <> "'")

editCategory (UserName admin) (Name category, CategoryId cid) = execOne
  "UPDATE categories SET name = ? WHERE id = ?"
  (category, cid)
  (Just $ admin <> " changed category " <> showText cid <> " to '" <> category <> "'")

deleteCategory (UserName admin) (CategoryId cid) = execOne
  "DELETE FROM categories WHERE id = ?"
  [cid]
  (Just $ admin <> " deleted category " <> showText cid)
