{-# LANGUAGE
    QuasiQuotes
  #-}

module API.Posts where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.Database         ( DbAccess(..)
                                                , paginate
                                                , PGArray(..)
                                                )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
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
                                                , opt
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
  :: DbAccess m
  => Author
  -> Id
  -> m AppResponse
getDraft (Author author) (Id id) =
  either (const BadRequest) (AppOk . J.toJSON) <$> M.getUnpublishedPost id author

getDrafts (Author author) (Page page) = AppOk . paginate <$> M.getDrafts author page

newtype PostEssential = PostEssential M.PostEssential

instance FromQuery PostEssential where
  parseQuery = fmap PostEssential $ M.PostEssential
    <$> param "title"
    <*> (liftMaybe . readT =<< param "category_id")
    <*> param "content"
    <*> param "main_image"
    <*> (fmap PGArray . liftMaybe . readT =<< param "images")

createPost
  :: (HasLog m, DbAccess m)
  => Author
  -> PostEssential
  -> m AppResponse
createPost (Author author) (PostEssential entity@M.PostEssential{..}) = do
  pid <- M.createPost author entity
  case pid of
    Left _ -> return BadRequest
    Right pid -> do
      log' Info $ author <> " created post " <> showText pid <> " titled '" <> title <> "'"
      return . AppOk . J.Number . fromInteger . toInteger $ pid

data TagPostRelation = TagPostRelation M.TagPostRelation

instance FromQuery TagPostRelation where
  parseQuery = fmap TagPostRelation $ M.TagPostRelation
    <$> (liftMaybe . readT =<< param "tag_id")
    <*> (liftMaybe . readT =<< param "post_id")

attachTag (Author author) (TagPostRelation rel@M.TagPostRelation{..}) = do
  res <- M.attachTag rel author
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ author <> " attached tag " <> showText tag_id <> " to post " <> showText post_id
      return (AppOk J.Null)

deattachTag (Author author) (TagPostRelation rel@M.TagPostRelation{..}) = do
  res <- M.deattachTag rel author
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ author <> " deattached tag " <> showText tag_id <> " to post " <> showText post_id
      return (AppOk J.Null)

newtype PostPartial = PostPartial M.PostPartial

instance FromQuery PostPartial where
  parseQuery =
    fmap PostPartial
      $   M.PostPartial
      <$> opt "title"
      <*> (liftMaybe . maybe (Just Nothing) (fmap Just . readT) =<< opt "category_id")
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
editPost (Author author) (Id pid, PostPartial entity@M.PostPartial{..}) = do
  res <- M.editPost author entity
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ author <> " edited post " <> showText pid
      return (AppOk J.Null)

publishPost
  :: (HasLog m, DbAccess m)
  => Author
  -> Id
  -> m AppResponse
publishPost (Author author) (Id pid) = do
  res <- M.publishPost author pid
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ author <> " published post " <> showText pid
      return (AppOk J.Null)

deletePost
  :: (HasLog m, DbAccess m)
  => Author
  -> Id
  -> m AppResponse
deletePost (Author author) (Id pid) = do
  res <- M.deletePost author pid
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ author <> " deleted post " <> showText pid
      return (AppOk J.Null)
