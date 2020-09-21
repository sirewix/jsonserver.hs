{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , PartialTypeSignatures
  , TypeApplications
  #-}

module Entry where

import           App.Response                   ( AppResponse(..))
import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Auth             ( Secrets
                                                , JWTVerification(..)
                                                )
import           App.Prototype.Database         ( DbAccess(..) )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
import           App.Implementation.Auth        ( generateJWT
                                                , runJWT
                                                , verifyJWT
                                                )
import           Config                         ( Config(..) )
import           Control.Monad                  ( void )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Text                      ( Text )
import           Entities                       ( UserName(..)
                                                , Token(..)
                                                , PostId(..)
                                                )
import           Misc                           ( showText
                                                , readT
                                                )
import           Network.Wai                    ( Request(..) )
import           FromQuery                      ( FromQuery(..) )
import qualified API.Login                     as Login
import qualified API.Authors                   as Authors
import qualified API.Categories                as Categories
import qualified API.Comments                  as Comments
import qualified API.Posts                     as Posts
import qualified API.Search                    as Search
import qualified API.Tags                      as Tags
import qualified API.Users                     as Users

entry
  :: ( HasEnv Config m
     , HasEnv Secrets m
     , HasLog m
     , DbAccess m
     , MonadIO m
     , MonadError Text m
     )
  => Request
  -> m AppResponse
entry req = do
  config <- getEnv @Config
  log' Debug $ showText req
  case pathInfo req of
    ["register"      ]        -> public Login.register
    ["login"         ]        -> public (Login.login $ \arg -> generateJWT (secrets_update_interval config * 60) arg <$> runJWT)

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
    ["createUser"    ]        -> admin Users.createUser
    ["deleteUser"    ]        -> admin Users.deleteUser

    ["getPost"       ]        -> author Posts.getPost
    ["getPosts"      ]        -> author Posts.getPosts
    ["createPost"    ]        -> author Posts.createPost
    ["editPost"      ]        -> author Posts.editPost
    ["publishPost"   ]        -> author Posts.publishPost
    ["deletePost"    ]        -> author Posts.deletePost

    ["attachTag"     ]        -> author Posts.attachTag
    ["deattachTag"   ]        -> author Posts.deattachTag

    ["post"          ]        -> return BadRequest
    ["post", pid     ]        -> path public pid (Posts.post . PostId)
    ["post", pid, "comments"] -> path public pid (Comments.getComments . PostId)
    ["posts"         ]        -> public Search.posts

    ["addComment"    ]        -> user Comments.addComment
    ["deleteComment" ]        -> user Comments.deleteComment

    _                         -> return NotFound
 where
  admin, author, user
    :: ( FromQuery q
       , HasEnv Config m
       , HasEnv Secrets m
       , HasLog m
       , DbAccess m
       , MonadIO m
       , MonadError Text m
       )
    => (UserName -> q -> m AppResponse) -> m AppResponse
  admin  = needToken (Just "admin")
  author = needToken (Just "author")
  user   = needToken Nothing

  needToken
    :: ( FromQuery q
       , HasEnv Config m
       , HasEnv Secrets m
       , HasLog m
       , DbAccess m
       , Monad m
       , MonadIO m
       , MonadError Text m
       )
    => Maybe Text
    -> (UserName -> q -> m AppResponse)
    -> m AppResponse
  needToken claim f = case parseQuery (queryString req) of
    Just (Just (Token token), q) -> do
      jwt <- verifyJWT claim token <$> runJWT
      case jwt of
        JWTOk username -> f username q
        JWTExp         -> return TokenExpired
        JWTReject      -> return NotFound
    Just (Nothing, q) -> getEnv >>= \config ->
      if backdoor config
         then f (UserName "admin") q
         else return NotFound
    Nothing -> return NotFound

  path which x f = maybe (return BadRequest) (which . f) (readT x)

  public
    :: ( FromQuery q
       , HasEnv Config m
       , HasEnv Secrets m
       , HasLog m
       , DbAccess m
       , Monad m
       , MonadIO m
       , MonadError Text m
       )
    => (q -> m AppResponse)
    -> m AppResponse
  public f = maybe (return BadRequest) f (parseQuery $ queryString req)

dbrefresh :: (Functor m, DbAccess m) => m ()
dbrefresh = void $ execute "REFRESH MATERIALIZED VIEW posts_view;" ()
