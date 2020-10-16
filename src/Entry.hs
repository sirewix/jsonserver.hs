{-# LANGUAGE ConstraintKinds #-}
module Entry where

import           App.Response                   ( AppResponse(..))
import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Auth             ( Admin(..)
                                                , Author(..)
                                                , User(..)
                                                , Secrets
                                                , JWTVerification(..)
                                                )
import           App.Prototype.Database         ( DbAccess(..) )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
import           App.Implementation.Auth        ( runJWT
                                                , verifyJWT
                                                )
import           Config                         ( Config(..) )
import           Control.Monad                  ( void )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Text                      ( Text )
import           Misc                           ( showText
                                                , readT
                                                )
import           Network.Wai                    ( Request(..) )
import           Query.Common                   ( Id(..)
                                                , Token(..)
                                                )
import           Query.FromQuery                ( FromQuery(..)
                                                , unQueryParser
                                                )
import qualified API.Authors                   as Authors
import qualified API.Categories                as Categories
import qualified API.Comments                  as Comments
import qualified API.Posts                     as Posts
import qualified API.Search                    as Search
import qualified API.Tags                      as Tags
import qualified API.Users                     as Users

type App m =
  ( HasEnv Config m
  , HasEnv Secrets m
  , HasLog m
  , DbAccess m
  , Monad m
  , MonadIO m
  , MonadError Text m
  )

entry
  :: App m
  => Request
  -> m AppResponse
entry req = do
  log' Debug $ showText req
  case pathInfo req of
    ["register"      ]        -> public Users.register
    ["login"         ]        -> public Users.login

    ["makeAuthor"    ]        -> admin Authors.makeAuthor
    ["getAuthors"    ]        -> admin Authors.getAuthors
    ["editAuthor"    ]        -> admin Authors.editAuthor
    ["deleteAuthor"  ]        -> admin Authors.deleteAuthor

    ["getTags"       ]        -> public Tags.getTags
    ["createTag"     ]        -> admin Tags.createTag
    ["editTag"       ]        -> admin Tags.editTag
    ["deleteTag"     ]        -> admin Tags.deleteTag

    ["getCategories" ]        -> public Categories.getCategories
    ["createCategory"]        -> admin Categories.createCategory
    ["editCategory"  ]        -> admin Categories.editCategory
    ["deleteCategory"]        -> admin Categories.deleteCategory

    ["getUsers"      ]        -> public Users.getUsers
    ["deleteUser"    ]        -> admin Users.deleteUser

    ["getDraft"      ]        -> author Posts.getDraft
    ["getDrafts"     ]        -> author Posts.getDrafts
    ["createPost"    ]        -> author Posts.createPost
    ["editPost"      ]        -> author Posts.editPost
    ["publishPost"   ]        -> author Posts.publishPost
    ["deletePost"    ]        -> author Posts.deletePost

    ["attachTag"     ]        -> author Posts.attachTag
    ["deattachTag"   ]        -> author Posts.deattachTag

    ["post"          ]        -> return BadRequest
    ["post", pid     ]        -> path public pid (Posts.post . Id)
    ["post", pid, "comments"] -> path public pid (Comments.getComments . Id)
    ["posts"         ]        -> public Search.posts

    ["addComment"    ]        -> user Comments.addComment
    ["deleteComment" ]        -> admin Comments.deleteComment

    _                         -> return NotFound
 where
  admin :: (FromQuery q, App m) => (Admin -> q -> m AppResponse) -> m AppResponse
  admin  = needToken Admin (Just "admin")

  author :: (FromQuery q, App m) => (Author -> q -> m AppResponse) -> m AppResponse
  author = needToken Author (Just "author")

  user :: (FromQuery q, App m) => (User -> q -> m AppResponse) -> m AppResponse
  user   = needToken User Nothing

  needToken
    :: (FromQuery q, App m)
    => (Text -> r)
    -> Maybe Text
    -> (r -> q -> m AppResponse)
    -> m AppResponse
  needToken role claim f = case unQueryParser parseQuery (queryString req) of
    Just (Just (Token token), q) -> do
      jwt <- verifyJWT claim token <$> runJWT
      case jwt of
        JWTOk username -> f (role username) q
        JWTExp         -> return TokenExpired
        JWTReject      -> return NotFound
    Just (Nothing, q) -> getEnv >>= \config ->
      if backdoor config
         then f (role "admin") q
         else return NotFound
    Nothing -> return NotFound

  path which x f = maybe (return BadRequest) (which . f) (readT x)

  public
    :: (FromQuery q, App m)
    => (q -> m AppResponse)
    -> m AppResponse
  public f = maybe (return BadRequest) f (unQueryParser parseQuery $ queryString req)

dbrefresh :: (Functor m, DbAccess m) => m ()
dbrefresh = void $ execute "REFRESH MATERIALIZED VIEW posts_view;" ()
