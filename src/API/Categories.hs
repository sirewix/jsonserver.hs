{-# LANGUAGE QuasiQuotes #-}

module API.Categories where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.App              ( HasEnv )
import           App.Prototype.Database         ( DbAccess(..)
                                                , paginate
                                                , unwrapRequest
                                                )
import           App.Prototype.Log              ( HasLog(..) )
import           App.Prototype.Auth             ( Admin(..) )
import           Config                         ( Config )
import           Misc                           ( showText )
import           Model.Categories               ( Category )
import           Query.Common                   ( QueryId(..)
                                                , Page(..)
                                                )
import           Query.FromQuery                ( FromQuery(..)
                                                , param
                                                , paramT
                                                , opt
                                                , optT
                                                )
import qualified Data.Aeson                    as J
import qualified Model.Categories              as M

newtype CategoryEssential = CategoryEssential M.CategoryEssential

instance FromQuery CategoryEssential where
  parseQuery = fmap CategoryEssential $ M.CategoryEssential
    <$> param "name"
    <*> paramT "parent_id"

newtype CategoryPartial = CategoryPartial M.CategoryPartial

instance FromQuery CategoryPartial where
  parseQuery = fmap CategoryPartial $ M.CategoryPartial
      <$> opt "name"
      <*> optT "parent_id"

getCategories :: (Monad m, DbAccess m, HasEnv Config m) => (Page, QueryId Category) -> m AppResponse
getCategories (Page page, QueryId id) = AppOk . paginate <$> M.getCategories id page

createCategory
  :: (HasLog m, DbAccess m)
  => Admin
  -> CategoryEssential
  -> m AppResponse
createCategory (Admin admin) (CategoryEssential entity@(M.CategoryEssential {..})) =
  M.createCategory entity >>= unwrapRequest BadRequest
    (J.toJSON)
    (Just $ \id -> admin <> " created category " <> showText id <> " '" <> name <> "'")

editCategory
  :: (HasLog m, DbAccess m)
  => Admin
  -> (QueryId Category, CategoryPartial)
  -> m AppResponse
editCategory (Admin admin) (QueryId cid, CategoryPartial entity@(M.CategoryPartial {..})) =
  M.editCategory cid entity >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ admin <> " changed category " <> showText cid)

deleteCategory
  :: (HasLog m, DbAccess m)
  => Admin
  -> QueryId Category
  -> m AppResponse
deleteCategory (Admin admin) (QueryId cid) =
  M.deleteCategory cid >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ admin <> " deleted category " <> showText cid)
