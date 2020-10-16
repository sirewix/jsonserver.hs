{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  #-}

module API.Tags where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.App              ( HasEnv )
import           App.Prototype.Database         ( DbAccess(..), paginate )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
import           App.Prototype.Auth             ( Admin(..) )
import           Config                         ( Config )
import           Misc                           ( showText )
import           Query.Common                   ( Id(..), Page(..) )
import           Query.FromQuery                ( FromQuery(..)
                                                , param
                                                )
import qualified Data.Aeson                    as J
import qualified Model.Tags                    as M

getTags
  :: (Monad m, DbAccess m, HasEnv Config m)
  => Page
  -> m AppResponse
getTags (Page page) = AppOk . paginate <$> M.getTags page

newtype TagEssential = TagEssential M.TagEssential

instance FromQuery TagEssential where
  parseQuery = TagEssential . M.TagEssential <$> param "tag"

createTag
  :: (DbAccess m, HasLog m)
  => Admin
  -> TagEssential
  -> m AppResponse
createTag (Admin admin) (TagEssential entity@(M.TagEssential tag)) = do
  res <- M.createTag entity
  case res of
    Left _ -> return BadRequest
    Right id -> do
      log' Info $ admin <> " created tag " <> showText id <> " '" <> tag <> "'"
      return . AppOk . J.Number . fromInteger . toInteger $ id

editTag
  :: (DbAccess m, HasLog m)
  => Admin
  -> (Id, TagEssential)
  -> m AppResponse
editTag (Admin admin) (Id id, TagEssential entity@(M.TagEssential tag)) = do
  res <- M.editTag id entity
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ admin <> " changed tag " <> showText id <> " to '" <> tag <> "'"
      return (AppOk J.Null)

deleteTag
  :: (DbAccess m, HasLog m)
  => Admin
  -> Id
  -> m AppResponse
deleteTag (Admin admin) (Id id) = do
  res <- M.deleteTag id
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ admin <> " deleted tag " <> showText id
      return (AppOk J.Null)
