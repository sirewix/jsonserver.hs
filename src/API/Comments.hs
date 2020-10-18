{-# LANGUAGE QuasiQuotes #-}

module API.Comments where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.App              ( HasEnv )
import           App.Prototype.Database         ( DbAccess(..)
                                                , paginate
                                                , unwrapRequest
                                                )
import           App.Prototype.Log              ( HasLog(..) )
import           App.Prototype.Auth             ( Admin(..)
                                                , User(..)
                                                )
import           Config                         ( Config )
import           Data.Text                      ( Text )
import           Misc                           ( showText )
import           Query.Common                   ( Id(..)
                                                , Page(..)
                                                )
import           Query.FromQuery                ( FromQuery(..)
                                                , filterQuery
                                                , param
                                                , paramT
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
    <$> paramT "post_id"
    <*> filterQuery (not . T.null) (param "comment")

addComment
  :: (HasLog m, DbAccess m)
  => User
  -> Comment
  -> m AppResponse
addComment (User user) (Comment {..}) = do
  let entity = M.CommentEssential
        { M.post_id = post_id
        , M.username = user
        , M.comment = comment
        }
  M.addComment entity >>= unwrapRequest BadRequest
    (J.Number . fromInteger . toInteger)
    (Just $ \id -> user <> " added comment " <> showText id <> " to post " <> showText post_id)

deleteComment
  :: (HasLog m, DbAccess m)
  => Admin
  -> Id
  -> m AppResponse
deleteComment (Admin user) (Id id) = do
  M.deleteComment id >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ user <> " deleted comment " <> showText id)
