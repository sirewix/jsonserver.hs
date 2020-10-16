{-# LANGUAGE QuasiQuotes #-}

module API.Comments where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.App              ( HasEnv )
import           App.Prototype.Database         ( DbAccess(..), paginate )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
import           App.Prototype.Auth             ( Admin(..)
                                                , User(..)
                                                )
import           Config                         ( Config )
import           Data.Text                      ( Text )
import           Misc                           ( readT
                                                , showText
                                                )
import           Query.Common                   ( Id(..), Page(..) )
import           Query.FromQuery                ( FromQuery(..)
                                                , filterQuery
                                                , liftMaybe
                                                , param
                                                )
import qualified Data.Aeson                    as J
import qualified Data.Text                     as T
import qualified Model.Comments                as M

getComments
  :: (Monad m, DbAccess m, HasEnv Config m)
  => Id
  -> Page
  -> m AppResponse
getComments (Id pid) (Page page) = AppOk . paginate <$> M.getComments pid page

data Comment = Comment
  { post_id :: Int
  , comment :: Text
  }

instance FromQuery Comment where
  parseQuery = Comment
    <$> (liftMaybe . readT =<< param "post_id")
    <*> filterQuery (not . T.null) (param "comment")

addComment
  :: (HasLog m, DbAccess m)
  => User
  -> Comment
  -> m AppResponse
addComment (User user) (Comment {..}) = do
  res <- M.addComment $ M.CommentEssential
    { M.post_id = post_id
    , M.username = user
    , M.comment = comment
    }
  case res of
    Left _ -> return BadRequest
    Right id -> do
      log' Info $ user <> " added comment " <> showText id <> " to post " <> showText post_id
      return . AppOk . J.Number . fromInteger . toInteger $ id

deleteComment
  :: (HasLog m, DbAccess m)
  => Admin
  -> Id
  -> m AppResponse
deleteComment (Admin user) (Id id) = do
  res <- M.deleteComment id
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ user <> " deleted comment " <> showText id
      return (AppOk J.Null)
