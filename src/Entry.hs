{-# LANGUAGE
  OverloadedStrings
, PartialTypeSignatures
#-}
module Entry (app) where

import           Authors
import           App
import           Auth
import           Misc
import           Entities
import           Data.Aeson                     ( (.=) )
import           Data.Functor
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple
                                         hiding ( Query )
import           Network.HTTP.Types             ( Status(..)
                                                , status200
                                                , status400
                                                , status401
                                                , status404
                                                , status500
                                                )
import           Network.Wai
import           Query
import qualified Data.Aeson                    as J
import qualified Data.Aeson.Types              as J
import           Data.Text(Text)
import           Logger
import           Tags

app :: Bool -> Secrets -> (Logger, Connection) -> Application
app backdoorOn secrets env@(log, _) req respond = do
  log Debug $ showText req
  case pathInfo req of
    -- ["posts"   ] -> posts req respond
    ["register"     ] -> public register
    ["login"] -> public (login $ \arg -> generateJWT arg <$> runJWT secrets)

    ["make_author"  ] -> admin make_author
    ["get_authors"  ] -> admin get_authors
    ["edit_author"  ] -> admin edit_author
    ["delete_author"] -> admin delete_author

    ["get_tags"     ] -> public get_tags
    ["create_tag"   ] -> admin create_tag
    ["edit_tag"     ] -> admin edit_tag
    ["delete_tag"   ] -> admin delete_tag
    _               -> respond $ err status400
 where
  admin
    :: Query arg
    => (UserName -> arg -> (Logger, Connection) -> IO AppResponse)
    -> IO ResponseReceived
  admin = public . needToken (Just "admin")
  --author = public . needToken (Just "author")
  needToken
    :: Query arg
    => Maybe Text
    -> (UserName -> arg -> (Logger, Connection) -> IO AppResponse)
    -> (Token, arg, Maybe BackdooredUser)
    -> (Logger, Connection)
    -> IO AppResponse
  needToken claim f (Token token, arg, backdoorUser) _ = if backdoorOn
    then f (UserName $ maybe "admin" unBackdooredUser backdoorUser) arg env
    else do
      jwt <- verifyJWT claim token <$> runJWT secrets
      case jwt of
        JWTOk username -> f username arg env
        JWTExp         -> return TokenExpired
        JWTReject      -> return AccessDenied

  public :: Query q => (q -> (Logger, Connection) -> IO AppResponse) -> IO ResponseReceived
  public f = respond =<< case parseQuery (queryString req) of
    Just q -> f q env <&> \res -> case res of
      AppOk txt     -> ok txt
      BadRequest    -> err status400
      InternalError -> err status500
      AccessDenied  -> err status400 -- hiding unauthorized apis as 400
      TokenExpired ->
        err $ Status { statusCode = 700, statusMessage = "Token expired" }
    Nothing -> return $ err status400
  json status x =
    responseLBS status [("Content-Type", "application/json")]
      . J.encode
      . J.object
      $ x
  ok x = json status200 $ ["ok" .= True, "response" .= x]
  err status =
    json status
      $ [ "ok" .= False
        , "code" .= statusCode status
        , "error" .= (decodeUtf8 $ statusMessage status)
        ]

posts :: Application
posts req respond = undefined
