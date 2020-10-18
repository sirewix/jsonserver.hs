{-# LANGUAGE
    QuasiQuotes
  #-}

module API.Posts where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.Database         ( DbAccess(..)
                                                , paginate
                                                , unwrapRequest
                                                , PGArray(..)
                                                )
import           App.Prototype.Log              ( HasLog(..) )
import           App.Prototype.Auth             ( Author(..) )
import           Misc                           ( readT
                                                , showText
                                                )
import           Query.Common                   ( Id(..)
                                                , Page(..)
                                                )
import           Query.FromQuery                ( FromQuery(..)
                                                , liftMaybe
                                                , param
                                                , paramT
                                                , opt
                                                , optT
                                                )
import qualified Data.Aeson                    as J
import qualified Model.Posts                   as M

post
  :: DbAccess m
  => Id
  -> ()
  -> m AppResponse
post (Id id) () = either (const BadRequest) (AppOk . J.toJSON) <$> M.getPublishedPost id

getDraft
  :: (HasLog m, DbAccess m)
  => Author
  -> Id
  -> m AppResponse
getDraft (Author author) (Id id) =
  M.getUnpublishedPost id author >>= unwrapRequest BadRequest
    J.toJSON
    Nothing

getDrafts (Author author) (Page page) = AppOk . paginate <$> M.getDrafts author page

newtype PostEssential = PostEssential M.PostEssential

instance FromQuery PostEssential where
  parseQuery = fmap PostEssential $ M.PostEssential
    <$> param "title"
    <*> paramT "category_id"
    <*> param "content"
    <*> param "main_image"
    <*> (PGArray <$> paramT "images")

createPost
  :: (HasLog m, DbAccess m)
  => Author
  -> PostEssential
  -> m AppResponse
createPost (Author author) (PostEssential entity@M.PostEssential{..}) =
  M.createPost author entity >>= unwrapRequest BadRequest
    (J.Number . fromInteger . toInteger)
    (Just $ \pid -> author <> " created post " <> showText pid <> " titled '" <> title <> "'")

data TagPostRelation = TagPostRelation M.TagPostRelation

instance FromQuery TagPostRelation where
  parseQuery = fmap TagPostRelation $ M.TagPostRelation
    <$> (paramT "tag_id")
    <*> (paramT "post_id")

attachTag (Author author) (TagPostRelation rel@M.TagPostRelation{..}) =
  M.attachTag rel author >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ author <> " attached tag " <> showText tag_id <> " to post " <> showText post_id)

deattachTag (Author author) (TagPostRelation rel@M.TagPostRelation{..}) =
  M.deattachTag rel author >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ author <> " deattached tag " <> showText tag_id <> " to post " <> showText post_id)

newtype PostPartial = PostPartial M.PostPartial

instance FromQuery PostPartial where
  parseQuery =
    fmap PostPartial
      $   M.PostPartial
      <$> opt "title"
      <*> optT "category_id"
      <*> opt "content"
      <*> opt "main_image"
      <*> (   fmap (fmap PGArray)
          .   liftMaybe
          .   maybe (Just Nothing) (fmap Just . readT)
          =<< opt "images"
          )

editPost
  :: (HasLog m, DbAccess m)
  => Author
  -> (Id, PostPartial)
  -> m AppResponse
editPost (Author author) (Id pid, PostPartial entity@M.PostPartial{..}) =
  M.editPost pid author entity >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ author <> " edited post " <> showText pid)

publishPost
  :: (HasLog m, DbAccess m)
  => Author
  -> Id
  -> m AppResponse
publishPost (Author author) (Id pid) =
  M.publishPost author pid >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ author <> " published post " <> showText pid)

deletePost
  :: (HasLog m, DbAccess m)
  => Author
  -> Id
  -> m AppResponse
deletePost (Author author) (Id pid) =
  M.deletePost author pid >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ author <> " deleted post " <> showText pid)
