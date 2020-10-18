{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  #-}

module API.Tags where

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
createTag (Admin admin) (TagEssential entity@(M.TagEssential tag)) =
  M.createTag entity >>= unwrapRequest BadRequest
    (J.Number . fromInteger . toInteger)
    (Just $ \id -> admin <> " created tag " <> showText id <> " '" <> tag <> "'")

editTag
  :: (DbAccess m, HasLog m)
  => Admin
  -> (Id, TagEssential)
  -> m AppResponse
editTag (Admin admin) (Id id, TagEssential entity@(M.TagEssential tag)) =
  M.editTag id entity >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ admin <> " changed tag " <> showText id <> " to '" <> tag <> "'")

deleteTag
  :: (DbAccess m, HasLog m)
  => Admin
  -> Id
  -> m AppResponse
deleteTag (Admin admin) (Id id) = do
  M.deleteTag id >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ admin <> " deleted tag " <> showText id)
