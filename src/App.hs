{-# LANGUAGE OverloadedStrings #-}

module App where

import           App.Response                   ( AppResponse(..) )
import           App.Implementation.App         ( App
                                                , Env
                                                , runLoggedApp
                                                )
import           Data.Aeson                     ( (.=) )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Network.HTTP.Types             ( Status(..)
                                                , status200
                                                , status400
                                                , status401
                                                , status404
                                                , status500
                                                )
import           Network.Wai                    ( Application
                                                , Request
                                                , responseLBS
                                                )
import qualified Data.Aeson                    as J

app
  :: (Request -> App AppResponse)
  -> Env
  -> Application
app entry env req respond = respond . toHttp =<< runLoggedApp (entry req) env InternalError
 where
  toHttp res = case res of
    AppOk res     -> ok res
    BadRequest    -> err status400
    InternalError -> err status500
    AccessDenied  -> err status401
    NotFound      -> err status404
    TokenExpired  -> err $ Status { statusCode = 700, statusMessage = "Token expired" }

  json status x = responseLBS status [("Content-Type", "application/json")] . J.encode . J.object $ x

  ok x = json status200
    [ "ok" .= True
    , "response" .= x
    ]

  err status = json status
    [ "ok" .= False
    , "code" .= statusCode status
    , "error" .= decodeUtf8 (statusMessage status)
    ]

