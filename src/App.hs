{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App
    ( AppResponse(..)
    , defaultDbHandlers
    , catchDb
    , Endpoint
    , paginate
    , limit
    , offset
    ) where

import           Control.Exception
import           Data.Aeson ((.=))
import           Data.Text                      ( pack )
import           Data.Text.Encoding
import           Data.Yaml (array)
import           Database.PostgreSQL.Simple hiding ( Query )
import           Logger
import           Misc
import qualified Data.Aeson                    as J

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

paginate :: Int -> [(Int, J.Value)] -> AppResponse
paginate pageSize q = if null q
  then BadRequest
  else AppOk $ J.object
    [ "pages" .= ((fst (head q) + pageSize - 1) `quot` pageSize)
    , "content" .= array (map snd q) ]

limit :: Int -> String
limit pageSize = show pageSize
offset :: Int -> Int -> String
offset pageSize page = show ((page - 1) * pageSize)
