module Query.FromQuery
  ( FromQuery(..)
  , QueryParser(..)
  , param
  , opt
  , paramT
  , optT
  , liftMaybe
  , filterQuery
  )
where

import           Control.Applicative            ( Alternative(..) )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Text                      ( Text )
import           Data.ByteString                ( ByteString )
import           Control.Monad                  ( join )
import           Misc                           ( filterMaybe
                                                , readT
                                                )

newtype QueryParser a = QueryParser { unQueryParser :: [(ByteString, Maybe ByteString)] -> Maybe a }

instance Functor QueryParser where
  fmap f (QueryParser p) = QueryParser (fmap f . p)

instance Applicative QueryParser where
  pure = QueryParser . const . Just
  (QueryParser f) <*> (QueryParser p) = QueryParser $ \q -> do
    a <- f q
    b <- p q
    return (a b)

instance Monad QueryParser where
  QueryParser a >>= f = QueryParser $ \q -> (a q) >>= ($ q) . unQueryParser . f

instance Alternative QueryParser where
  empty = QueryParser (const Nothing)
  QueryParser a <|> QueryParser b = QueryParser $ \q -> a q <|> b q

class FromQuery a where
  parseQuery :: QueryParser a

liftMaybe :: Maybe a -> QueryParser a
liftMaybe = QueryParser . const

filterQuery :: (a -> Bool) -> QueryParser a -> QueryParser a
filterQuery f (QueryParser p) = QueryParser (filterMaybe f . p)

param :: ByteString -> QueryParser Text
param p = decodeUtf8 <$> QueryParser (join . lookup p)

paramT :: Read a => ByteString -> QueryParser a
paramT p = liftMaybe . readT =<< param p

opt :: ByteString -> QueryParser (Maybe Text)
opt p = fmap decodeUtf8 <$> QueryParser (Just . join . lookup p)

optT :: Read a => ByteString -> QueryParser (Maybe a)
optT p = liftMaybe . maybe (Just Nothing) (fmap Just . readT) =<< opt p

instance FromQuery () where
  parseQuery = pure ()

instance FromQuery a => FromQuery (Maybe a) where
  parseQuery = QueryParser $ Just . (unQueryParser parseQuery)

instance (FromQuery a, FromQuery b) => FromQuery (a, b) where
  parseQuery = (,)
    <$> parseQuery
    <*> parseQuery

instance (FromQuery a, FromQuery b, FromQuery c) => FromQuery (a, b, c) where
  parseQuery = (,,)
    <$> parseQuery
    <*> parseQuery
    <*> parseQuery

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d) => FromQuery (a, b, c, d) where
  parseQuery = (,,,)
    <$> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d, FromQuery e) => FromQuery (a, b, c, d, e) where
  parseQuery = (,,,,)
    <$> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d, FromQuery e, FromQuery f) => FromQuery (a, b, c, d, e, f) where
  parseQuery = (,,,,,)
    <$> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d, FromQuery e, FromQuery f, FromQuery g) => FromQuery (a, b, c, d, e, f, g) where
  parseQuery = (,,,,,,)
    <$> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d, FromQuery e, FromQuery f, FromQuery g, FromQuery h) => FromQuery (a, b, c, d, e, f, g, h) where
  parseQuery = (,,,,,,,)
    <$> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d, FromQuery e, FromQuery f, FromQuery g, FromQuery h, FromQuery i) => FromQuery (a, b, c, d, e, f, g, h, i) where
  parseQuery = (,,,,,,,,)
    <$> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d, FromQuery e, FromQuery f, FromQuery g, FromQuery h, FromQuery i, FromQuery j) => FromQuery (a, b, c, d, e, f, g, h, i, j) where
  parseQuery = (,,,,,,,,,)
    <$> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
    <*> parseQuery
