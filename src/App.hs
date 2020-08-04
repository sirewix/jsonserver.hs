{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}
module App
  ( AppResponse(..)
  , Endpoint
  , catchDb
  , defaultDbHandlers
  , execdb
  , limit
  , offset
  , paginate
  , queryOne
  , queryPaged
  , dbrefresh
  )
where

import           Control.Exception
import           Control.Monad
import           Data.Aeson                     ( (.=) )
import           Data.Text                      ( pack )
import           Data.Text.Encoding
import           Data.Yaml                      ( array )
import           Database.PostgreSQL.Simple
                                         hiding ( Query )
import           Logger
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
  , Handler
    (\(e :: SqlError) ->
      log
          Error
          (decodeUtf8 $ sqlErrorMsg e <> " (" <> ") (" <> sqlErrorDetail e <> ") (" <> sqlErrorHint e <> ")")
        >> return InternalError
    )
  , Handler (\(e :: ResultError) -> log Error (pack $ showResultError e) >> return InternalError)
  ]

showResultError (Incompatible     sqlType _ _     hType msg) = msg <> " (" <> hType <> " ~ " <> sqlType <> ")"
showResultError (UnexpectedNull   _       _ field _     msg) = msg <> " @ " <> field
showResultError (ConversionFailed sqlType _ _     hType msg) = msg <> " (" <> hType <> " ~ " <> sqlType <> ")"

catchDb log ret = (`catches` (Handler (\(_ :: QueryError) -> ret) : defaultDbHandlers log))

type Endpoint = (Logger, Connection) -> IO AppResponse

paginate :: Int -> [(Int, J.Value)] -> AppResponse
paginate pageSize q =
  let (pages, content) = if null q
        then (0, J.Null)
        else ((fst (head q) + pageSize - 1) `quot` pageSize, array (map snd q))
  in  AppOk $ J.object ["pages" .= pages, "content" .= content]

limit :: Int -> String
limit = show

offset :: Int -> Int -> String
offset pageSize page = show ((page - 1) * pageSize)

execdb q fq msg (log, db) = catchDb log (return BadRequest) $ do
  dbres <- execute db q fq
  if dbres == 1
    then do
      forM_ msg (log Info)
      return . AppOk $ J.Null
    else return BadRequest

queryPaged pageSize q fq (log, db) = catchDb log (return InternalError) $ do
  r <- query db q fq :: IO [(Int, J.Value)]
  return $ paginate pageSize r

queryOne q fq g msg (log, db) = catchDb log (return BadRequest) $ do
  q <- query db q fq
  case q of
    [Only r] -> do
      forM_ msg (log Info . ($ r))
      return . AppOk $ g r
    _ -> return BadRequest

dbrefresh db = execute db "REFRESH MATERIALIZED VIEW posts_view;" ()

