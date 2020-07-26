{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App
    ( AppResponse(..)
    , defaultDbHandlers
    , catchDb
    , Endpoint
    ) where

import           Misc
import           Control.Exception
import           Database.PostgreSQL.Simple hiding ( Query )
import           Logger
import qualified Data.Aeson                    as J
import           Data.Text                      ( pack )
import           Data.Text.Encoding

data AppResponse =
    AppOk J.Value
  | BadRequest
  | InternalError
  | AccessDenied
  | TokenExpired
  | NotFound

defaultDbHandlers log =
  [ Handler (\(e :: FormatError) -> log Error (pack $ fmtMessage e) >> return InternalError)
  , Handler (\(e :: SqlError   ) -> log Error (decodeUtf8
                                                    $ sqlErrorMsg e <> " (" <> ") ("
                                                   <> sqlErrorDetail e <> ") ("
                                                   <> sqlErrorHint e <> ")") >> return InternalError)
  , Handler (\(e :: ResultError) -> log Error (pack $ showResultError e) >> return InternalError)
  ]

showResultError (Incompatible sqlType _ _ hType msg) =
  msg <> " (" <> hType <> " ~ " <> sqlType <> ")"

showResultError (UnexpectedNull _ _ field _ msg) =
  msg <> " @ " <> field

showResultError (ConversionFailed sqlType _ _ hType msg) =
  msg <> " (" <> hType <> " ~ " <> sqlType <> ")"

catchDb log ret = flip catches (Handler (\(e :: QueryError) -> ret) : defaultDbHandlers log)

type Endpoint = (Logger, Connection) -> IO AppResponse
