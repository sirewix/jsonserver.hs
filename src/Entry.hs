{-# LANGUAGE
    OverloadedStrings
  , PartialTypeSignatures
  #-}
module Entry where
import           App                            ( Endpoint
                                                , AppResponse(..)
                                                )
import           Auth                           ( Secrets
                                                , JWTVerification(..)
                                                , generateJWT
                                                , login
                                                , register
                                                , runJWT
                                                , verifyJWT
                                                )
import           Config                         ( Config(..) )
import           Data.Aeson                     ( (.=) )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Database.PostgreSQL.Simple     ( Connection )
import           Entities                       ( UserName(..)
                                                , Token(..)
                                                , PostId(..)
                                                )
import           Logger                         ( Logger
                                                , Priority(..)
                                                )
import           Misc                           ( showText
                                                , readT
                                                )
import           Network.HTTP.Types             ( Status(..)
                                                , status200
                                                , status400
                                                , status401
                                                , status404
                                                , status500
                                                )
import           Network.Wai                    ( Application
                                                , ResponseReceived
                                                , pathInfo
                                                , queryString
                                                , responseLBS
                                                )
import           Query                          ( Query(..) )
import qualified Authors
import qualified Categories
import qualified Comments
import qualified Data.Aeson                    as J
import qualified Posts
import qualified Search
import qualified Tags
import qualified Users

app :: Config -> Secrets -> (Logger, Connection) -> Application
app config secrets env@(log, _) req respond = do
  log Debug $ showText req
  case pathInfo req of
    ["register"       ]       -> public register
    ["login"          ]       -> public (login $ \arg -> generateJWT (toInteger $ secrets_update_interval config * 60) arg <$> runJWT secrets)

    ["makeAuthor"    ]       -> admin Authors.makeAuthor
    ["getAuthors"    ]       -> admin Authors.getAuthors
    ["editAuthor"    ]       -> admin Authors.editAuthor
    ["deleteAuthor"  ]       -> admin Authors.deleteAuthor

    ["getTags"       ]       -> public Tags.getTags
    ["createTag"     ]       -> admin Tags.createTag
    ["editTag"       ]       -> admin Tags.editTag
    ["deleteTag"     ]       -> admin Tags.deleteTag

    ["getCategories" ]       -> public Categories.getCategories
    ["createCategory"]       -> admin Categories.createCategory
    ["editCategory"  ]       -> admin Categories.editCategory
    ["deleteCategory"]       -> admin Categories.deleteCategory

    ["getUsers"      ]       -> public Users.getUsers
    ["createUser"    ]       -> admin Users.createUser
    ["deleteUser"    ]       -> admin Users.deleteUser

    ["getPost"       ]       -> author Posts.getPost
    ["getPosts"      ]       -> author Posts.getPosts
    ["createPost"    ]       -> author Posts.createPost
    ["editPost"      ]       -> author Posts.editPost
    ["publishPost"   ]       -> author Posts.publishPost
    ["deletePost"    ]       -> author Posts.deletePost

    ["attachTag"     ]       -> author Posts.attachTag
    ["deattachTag"   ]       -> author Posts.deattachTag

    ["post"           ]       -> respond $ toHttp BadRequest
    ["post", pid]             -> path public pid (Posts.post . PostId)
    ["post", pid, "comments"] -> path public pid (Comments.getComments . PostId)
    ["posts"         ]        -> public Search.posts

    ["addComment"   ]        -> user Comments.addComment
    ["deleteComment"]        -> user Comments.deleteComment

    _                         -> respond $ toHttp NotFound
 where
  admin, author, user :: Query arg => (UserName -> arg -> Endpoint) -> IO ResponseReceived
  admin  = needToken (Just "admin")
  author = needToken (Just "author")
  user   = needToken Nothing

  needToken :: Query arg => Maybe Text -> (UserName -> arg -> Endpoint) -> IO ResponseReceived
  needToken claim f = respond . toHttp =<< case parseQuery (queryString req) of
    Just (Just (Token token), arg) -> do
      jwt <- verifyJWT claim token <$> runJWT secrets
      case jwt of
        JWTOk username -> f username arg env
        JWTExp         -> return TokenExpired
        JWTReject      -> return NotFound
    Just (Nothing, arg) -> if backdoor config
                              then f (UserName "admin") arg env
                              else return NotFound
    Nothing             -> return NotFound

  path which x f = maybe (respond $ toHttp BadRequest) (which . f) (readT x)

  public :: Query q => (q -> Endpoint) -> IO ResponseReceived
  public f = respond . toHttp =<< case parseQuery (queryString req) of
    Just q  -> f q env
    Nothing -> return BadRequest

  toHttp res = case res of
    AppOk res     -> ok res
    BadRequest    -> err status400
    InternalError -> err status500
    AccessDenied  -> err status401
    NotFound      -> err status404
    TokenExpired  -> err $ Status { statusCode = 700, statusMessage = "Token expired" }

  json status x = responseLBS status [("Content-Type", "application/json")] . J.encode . J.object $ x

  ok x = json status200 ["ok" .= True, "response" .= x]

  err status =
    json status ["ok" .= False, "code" .= statusCode status, "error" .= decodeUtf8 (statusMessage status)]

