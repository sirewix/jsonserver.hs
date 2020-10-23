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
import           Query.Common                   ( QueryId(..)
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
    ["post", pid     ]        -> path public pid (Posts.post . QueryId)
    ["post", pid, "comments"] -> path public pid (Comments.getComments . QueryId)
    ["posts"         ]        -> public Search.posts

    ["addComment"    ]        -> user Comments.addComment
    ["deleteComment" ]        -> admin Comments.deleteComment

    _                         -> return NotFound
 where
  fromQuery :: FromQuery q => Maybe q
  fromQuery = unQueryParser parseQuery (queryString req)

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
  needToken role claim f = do
    config <- getEnv
    if backdoor config
       then proceedWithQuery (f $ role "admin")
       else maybe (return NotFound) proceedWithToken fromQuery
   where
    proceedWithQuery f = maybe (return BadRequest) f fromQuery
    proceedWithToken (Token token) = do
      jwt <- verifyJWT claim token <$> runJWT
      case jwt of
        JWTOk username -> proceedWithQuery (f $ role username)
        JWTExp         -> return TokenExpired
        JWTReject      -> return NotFound

  path which x f = maybe (return BadRequest) (which . f) (readT x)

  public
    :: (FromQuery q, App m)
    => (q -> m AppResponse)
    -> m AppResponse
  public f = maybe (return BadRequest) f fromQuery

dbrefresh :: (Functor m, DbAccess m) => m ()
dbrefresh = void $ execute "REFRESH MATERIALIZED VIEW posts_view;" ()
