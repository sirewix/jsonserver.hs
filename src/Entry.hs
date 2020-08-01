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
import           Data.ByteString.Lazy
import           Data.Functor
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
import qualified Data.Aeson.Types              as J

app :: Config -> Secrets -> (Logger, Connection) -> Application
app config secrets env@(log, _) req respond = do
  log Debug $ showText req
  case pathInfo req of
    ["register"       ]       -> public register
    ["login"          ]       -> public (login $ \arg -> generateJWT (toInteger $ secrets_update_interval config * 60) arg <$> runJWT secrets)

    ["make_author"    ]       -> admin make_author
    ["get_authors"    ]       -> admin get_authors
    ["edit_author"    ]       -> admin edit_author
    ["delete_author"  ]       -> admin delete_author

    ["get_tags"       ]       -> public get_tags
    ["create_tag"     ]       -> admin create_tag
    ["edit_tag"       ]       -> admin edit_tag
    ["delete_tag"     ]       -> admin delete_tag

    ["get_categories" ]       -> public get_categories
    ["create_category"]       -> admin create_category
    ["edit_category"  ]       -> admin edit_category
    ["delete_category"]       -> admin delete_category

    ["get_users"      ]       -> public get_users
    ["create_user"    ]       -> admin create_user
    ["delete_user"    ]       -> admin delete_user

    ["get_post"       ]       -> author get_post
    ["get_posts"      ]       -> author get_posts
    ["create_post"    ]       -> author create_post
    ["edit_post"      ]       -> author edit_post
    ["publish_post"   ]       -> author publish_post
    ["delete_post"    ]       -> author delete_post

    ["attach_tag"     ]       -> author attach_tag
    ["deattach_tag"   ]       -> author deattach_tag

    ["post"           ]       -> respond $ toHttp BadRequest
    ["post", pid]             -> path public pid (post . PostId)
    ["post", pid, "comments"] -> path public pid (get_comments . PostId)
    ["posts"         ]        -> public posts

    ["add_comment"   ]        -> user add_comment
    ["delete_comment"]        -> user delete_comment

    _                         -> respond $ toHttp NotFound
 where
  admin, author, user :: Query arg => (UserName -> arg -> Endpoint) -> IO ResponseReceived
  admin  = needToken (Just "admin")
  author = needToken (Just "author")
  user   = needToken Nothing

  needToken :: Query arg => Maybe Text -> (UserName -> arg -> Endpoint) -> IO ResponseReceived
  needToken claim f = respond =<< toHttp <$> case parseQuery (queryString req) of
    Just (Just (Token token), arg) -> do
      jwt <- verifyJWT claim token <$> runJWT secrets
      case jwt of
        JWTOk username -> f username arg env
        JWTExp         -> return TokenExpired
        JWTReject      -> return NotFound
    Just (Nothing, arg) -> if (backdoor config) then f (UserName "admin") arg env else return NotFound
    Nothing             -> return NotFound

  path which x f = maybe (respond $ toHttp BadRequest) (which . f) (readT x)

  public :: Query q => (q -> Endpoint) -> IO ResponseReceived
  public f = respond =<< toHttp <$> case parseQuery (queryString req) of
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

  ok x = json status200 $ ["ok" .= True, "response" .= x]

  err status =
    json status $ ["ok" .= False, "code" .= statusCode status, "error" .= (decodeUtf8 $ statusMessage status)]

