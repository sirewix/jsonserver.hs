{-# LANGUAGE
    OverloadedStrings
  , PartialTypeSignatures
  #-}
module Entry where
import           App
import           Auth
import           Authors
import           Categories
import           Comments
import           Config
import           Data.Aeson                     ( (.=) )
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple
                                         hiding ( Query )
import           Entities
import           Logger
import           Misc
import           Network.HTTP.Types             ( Status(..)
                                                , status200
                                                , status400
                                                , status401
                                                , status404
                                                , status500
                                                )
import           Network.Wai
import           Posts
import           Query
import           Search
import           Tags
import           Users
import qualified Data.Aeson                    as J

app :: Config -> Secrets -> (Logger, Connection) -> Application
app config secrets env@(log, _) req respond = do
  log Debug $ showText req
  case pathInfo req of
    ["register"       ]       -> public register
    ["login"          ]       -> public (login $ \arg -> generateJWT (toInteger $ secrets_update_interval config * 60) arg <$> runJWT secrets)

    ["makeAuthor"    ]       -> admin makeAuthor
    ["getAuthors"    ]       -> admin getAuthors
    ["editAuthor"    ]       -> admin editAuthor
    ["deleteAuthor"  ]       -> admin deleteAuthor

    ["getTags"       ]       -> public getTags
    ["createTag"     ]       -> admin createTag
    ["editTag"       ]       -> admin editTag
    ["deleteTag"     ]       -> admin deleteTag

    ["getCategories" ]       -> public getCategories
    ["createCategory"]       -> admin createCategory
    ["editCategory"  ]       -> admin editCategory
    ["deleteCategory"]       -> admin deleteCategory

    ["getUsers"      ]       -> public getUsers
    ["createUser"    ]       -> admin createUser
    ["deleteUser"    ]       -> admin deleteUser

    ["getPost"       ]       -> author getPost
    ["getPosts"      ]       -> author getPosts
    ["createPost"    ]       -> author createPost
    ["editPost"      ]       -> author editPost
    ["publishPost"   ]       -> author publishPost
    ["deletePost"    ]       -> author deletePost

    ["attachTag"     ]       -> author attachTag
    ["deattachTag"   ]       -> author deattachTag

    ["post"           ]       -> respond $ toHttp BadRequest
    ["post", pid]             -> path public pid (post . PostId)
    ["post", pid, "comments"] -> path public pid (getComments . PostId)
    ["posts"         ]        -> public posts

    ["addComment"   ]        -> user addComment
    ["deleteComment"]        -> user deleteComment

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

