{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  #-}

module App.Prototype.Database
  ( DbAccess(..)
  , (:.)(..)
  , FromField
  , FromRow(..)
  , Id(..)
  , Only(..)
  , Page
  , Paged(..)
  , Query(..)
  , ToField(..)
  , ToRow(..)
  , PGArray(..)
  , execOne
  , field
  , limit
  , offset
  , paginate
  , unwrapRequest
  , queryOne
  , queryPaged
  , sql
  )
where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
import           Control.Arrow                  ( first )
import           Control.Monad.Except           ( MonadError(..)
                                                , liftEither
                                                )
import           Data.Aeson                     ( (.=) )
import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Database.PostgreSQL.Simple     ( Only(..)
                                                , (:.)(..)
                                                )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow(..) )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..) )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.Types
                                                ( Query(..)
                                                , PGArray(..)
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import qualified Data.Aeson                    as J


newtype Id a = Id { getId :: Int }
  deriving (ToField, FromField)

instance Show (Id a) where show = show . getId
instance Read (Id a) where readsPrec d = map (first Id) . readsPrec d
instance J.FromJSON (Id a) where parseJSON = fmap Id . J.parseJSON
instance J.ToJSON (Id a) where toJSON = J.toJSON . getId

type Page = Int

class MonadError Text m => DbAccess m where
  execute    :: ToRow q => Query -> q -> m (Either Text Int64)
  query      :: (ToRow q, FromRow r) => Query -> q -> m (Either Text [r])

execOne
  :: (DbAccess m, MonadError Text m, ToRow fq)
  => Query
  -> fq
  -> m (Either Text ())
execOne q fq = execute q fq >>= either (return . Left) (f . (`compare` 1))
  where f EQ = return $ Right ()
        f GT = throwError $ "sql execution affected more than one row (" <> textQuery <> ")"
        f LT = return $ Left $ "no rows affected (" <> textQuery <> ")"
        textQuery = decodeUtf8 (fromQuery q)

unwrapRequest
  :: (HasLog m, DbAccess m)
  => AppResponse
  -> (r -> J.Value)
  -> Maybe (r -> Text)
  -> Either Text r
  -> m AppResponse
unwrapRequest  err _wopes _msg (Left e) = log' Debug ("bad request: " <> e) >> return err
unwrapRequest _err  wopes  msg (Right res) = mapM_ (log' Info . ($ res)) msg >> return (AppOk $ wopes res)

data Paged r = Paged Int [r]

instance Functor Paged where
  fmap f (Paged p a) = Paged p (map f a)

instance Foldable Paged where
  foldMap f (Paged _ a) = foldMap f a

instance Traversable Paged where
  traverse f (Paged p a) = Paged p <$> traverse f a

queryPaged
  :: (DbAccess m, ToRow fq, FromRow r)
  => Int
  -> Query
  -> fq
  -> m (Paged r)
queryPaged pageSize q fq = fmap f . liftEither =<< query q fq
 where
  f res = if null res
    then Paged 0 []
    else Paged ((fromOnly (fst (head res)) + pageSize - 1) `quot` pageSize) (map snd res)
  fst (a :. _) = a
  snd (_ :. b) = b

paginate :: J.ToJSON a => Paged a -> J.Value
paginate (Paged pages content) =
  J.object ["pages" .= pages, "content" .= J.toJSONList content]

queryOne
  :: (DbAccess m, MonadError Text m, ToRow fq, FromField r)
  => Query
  -> fq
  -> m (Either Text r)
queryOne q fq = query q fq >>= either (return . Left) f
 where
  f [Only r] = return $ Right r
  f []       = return $ Left "sql query returned zero rows"
  f _ =
    throwError
      $  "sql query returned more than one row ("
      <> decodeUtf8 (fromQuery q)
      <> ")"

limit :: Int -> Int
limit = id

offset :: Int -> Int -> Int
offset pageSize page = (page - 1) * pageSize
