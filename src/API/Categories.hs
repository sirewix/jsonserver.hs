{-# LANGUAGE QuasiQuotes #-}

module API.Categories where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.App              ( HasEnv )
import           App.Prototype.Database         ( DbAccess(..), paginate )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
import           App.Prototype.Auth             ( Admin(..) )
import           Config                         ( Config )
import           Misc                           ( readT
                                                , readNullable
                                                , showText
                                                )
import           Query.Common                   ( Id(..), Page(..) )
import           Query.FromQuery                ( FromQuery(..)
                                                , liftMaybe
                                                , param
                                                , opt
                                                )
import qualified Data.Aeson                    as J
import qualified Model.Categories              as M

newtype CategoryEssential = CategoryEssential M.CategoryEssential

instance FromQuery CategoryEssential where
  parseQuery = fmap CategoryEssential . M.CategoryEssential
    <$> param "name"
    <*> (liftMaybe . maybe (Just Nothing) (fmap Just . readT) =<< opt "parent_id")

newtype CategoryPartial = CategoryPartial M.CategoryPartial

instance FromQuery CategoryPartial where
  parseQuery = fmap CategoryPartial . M.CategoryPartial
    <$> opt "name"
    <*> (liftMaybe . maybe (Just Nothing) (fmap Just . readNullable) =<< opt "parent_id")

getCategories :: (Monad m, DbAccess m, HasEnv Config m) => (Page, Maybe Id) -> m AppResponse
getCategories (Page page, mbcid) = AppOk . paginate <$> M.getCategories (unId <$> mbcid) page

createCategory
  :: (HasLog m, DbAccess m)
  => Admin
  -> CategoryEssential
  -> m AppResponse
createCategory (Admin admin) (CategoryEssential entity@(M.CategoryEssential {..})) = do
  cid <- M.createCategory entity
  case cid of
    Left _ -> return BadRequest
    Right cid -> do
      log' Info (admin <> " created category " <> showText cid <> " '" <> name <> "'")
      return . AppOk . J.Number . fromInteger . toInteger $ cid

editCategory
  :: (HasLog m, DbAccess m)
  => Admin
  -> (Id, CategoryPartial)
  -> m AppResponse
editCategory (Admin admin) (Id cid, CategoryPartial entity@(M.CategoryPartial {..})) = do
  res <- M.editCategory cid entity
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ admin <> " changed category " <> showText cid
      return (AppOk J.Null)

deleteCategory
  :: (HasLog m, DbAccess m)
  => Admin
  -> Id
  -> m AppResponse
deleteCategory (Admin admin) (Id cid) = do
  res <- M.deleteCategory cid
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ admin <> " deleted category " <> showText cid
      return (AppOk J.Null)
