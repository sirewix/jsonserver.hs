{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module App.Prototype.Database
  ( DbAccess(..)
  , FromField
  , FromRow
  , Only(..)
  , Query(..)
  , ToRow
  , execOne
  , limit
  , offset
  , queryOne
  , queryPaged
  , sql
  )
where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
import           Control.Monad                  ( forM_ )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Aeson                     ( (.=) )
import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Yaml                      ( array )
import           Database.PostgreSQL.Simple     ( ToRow
                                                , FromRow
                                                , Only(..)
                                                )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField )
import           Database.PostgreSQL.Simple.Types
                                                ( Query(..) )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import qualified Data.Aeson                    as J

class DbAccess m where
  execute    :: ToRow q => Query -> q -> m Int64
  query      :: (ToRow q, FromRow r) => Query -> q -> m [r]

execOne
  :: (HasLog m, DbAccess m, MonadError Text m, ToRow fq)
  => Query
  -> fq
  -> Maybe Text
  -> m AppResponse
execOne q fq msg = execute q fq >>= f . (1 `compare`)
  where f EQ = do forM_ msg (log' Info)
                  return . AppOk $ J.Null
        f GT = throwError $ "sql execution affected more than one row (" <> decodeUtf8 (fromQuery q) <> ")"
        f LT = return BadRequest

queryPaged
  :: (Functor m, DbAccess m, ToRow fq)
  => Int
  -> Query
  -> fq
  -> m AppResponse
queryPaged pageSize q fq = AppOk . paginate pageSize <$> query q fq

queryOne
  :: (HasLog m, DbAccess m, MonadError Text m, ToRow fq, FromField r)
  => Query
  -> fq
  -> (r -> J.Value)
  -> Maybe (r -> Text)
  -> m AppResponse
queryOne q fq g msg = query q fq >>= f
  where f [Only r] = do forM_ msg (log' Info . ($ r))
                        return . AppOk $ g r
        f [] = return BadRequest
        f _  = throwError $ "sql query returned more than one row or column (" <> decodeUtf8 (fromQuery q) <> ")"

paginate :: Int -> [(Int, J.Value)] -> J.Value
paginate pageSize q =
  let (pages, content) = if null q
        then (0, J.Null)
        else ((fst (head q) + pageSize - 1) `quot` pageSize, array (map snd q))
  in  J.object ["pages" .= pages, "content" .= content]

limit :: Int -> Int
limit = id

offset :: Int -> Int -> Int
offset pageSize page = (page - 1) * pageSize
