{-# LANGUAGE QuasiQuotes #-}

module API.Comments where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.App              ( HasEnv )
import           App.Prototype.Database         ( DbAccess(..)
                                                , Id
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
import           Model.Posts                    ( Post )
import           Model.Comments                 ( Comment )
import           Query.Common                   ( QueryId(..)
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
  => QueryId Post
  -> Page
  -> m AppResponse
getComments (QueryId pid) (Page page) = AppOk . paginate <$> M.getComments pid page

data NewComment = NewComment
  { post_id :: Id Post
  , comment :: Text
  }

instance FromQuery NewComment where
  parseQuery = NewComment
    <$> paramT "post_id"
    <*> filterQuery (not . T.null) (param "comment")

addComment
  :: (HasLog m, DbAccess m)
  => User
  -> NewComment
  -> m AppResponse
addComment (User user) (NewComment {..}) = do
  let entity = M.CommentEssential
        { M.post_id = post_id
        , M.username = user
        , M.comment = comment
        }
  M.addComment entity >>= unwrapRequest BadRequest
    J.toJSON
    (Just $ \id -> user <> " added comment " <> showText id <> " to post " <> showText post_id)

deleteComment
  :: (HasLog m, DbAccess m)
  => Admin
  -> QueryId Comment
  -> m AppResponse
deleteComment (Admin user) (QueryId id) = do
  M.deleteComment id >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ user <> " deleted comment " <> showText id)
