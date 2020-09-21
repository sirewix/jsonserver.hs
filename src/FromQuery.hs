{-# LANGUAGE OverloadedStrings #-}
module FromQuery
  ( FromQuery(..)
  , (.:)
  , (.:?)
  )
where

import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Text                      ( Text )
import           Data.ByteString                ( ByteString )
import           Control.Monad                  ( join )

type QueryParser a = [(ByteString, Maybe ByteString)] -> Maybe a

(.:) :: ByteString -> QueryParser Text
q .: xs = decodeUtf8 <$> join (lookup q xs)
(.:?) :: ByteString -> QueryParser (Maybe Text)
q .:? xs = fmap decodeUtf8 <$> lookup q xs

class FromQuery a where
  parseQuery :: QueryParser a

instance FromQuery () where
  parseQuery _ = Just ()

instance FromQuery a => FromQuery (Maybe a) where
  parseQuery = Just . parseQuery

instance (FromQuery a, FromQuery b) => FromQuery (a, b) where
  parseQuery q = do
    a <- parseQuery q
    b <- parseQuery q
    return (a, b)

instance (FromQuery a, FromQuery b, FromQuery c) => FromQuery (a, b, c) where
  parseQuery q = do
    (a, b) <- parseQuery q
    c      <- parseQuery q
    return (a, b, c)

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d) => FromQuery (a, b, c, d) where
  parseQuery q = do
    (a, b, c) <- parseQuery q
    d         <- parseQuery q
    return (a, b, c, d)

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d, FromQuery e) => FromQuery (a, b, c, d, e) where
  parseQuery q = do
    (a, b, c, d) <- parseQuery q
    e            <- parseQuery q
    return (a, b, c, d, e)

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d, FromQuery e, FromQuery f) => FromQuery (a, b, c, d, e, f) where
  parseQuery q = do
    (a, b, c, d, e) <- parseQuery q
    f               <- parseQuery q
    return (a, b, c, d, e, f)

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d, FromQuery e, FromQuery f, FromQuery g) => FromQuery (a, b, c, d, e, f, g) where
  parseQuery q = do
    (a, b, c, d, e, f) <- parseQuery q
    g                  <- parseQuery q
    return (a, b, c, d, e, f, g)

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d, FromQuery e, FromQuery f, FromQuery g, FromQuery h) => FromQuery (a, b, c, d, e, f, g, h) where
  parseQuery q = do
    (a, b, c, d, e, f, g) <- parseQuery q
    h                     <- parseQuery q
    return (a, b, c, d, e, f, g, h)

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d, FromQuery e, FromQuery f, FromQuery g, FromQuery h, FromQuery i) => FromQuery (a, b, c, d, e, f, g, h, i) where
  parseQuery q = do
    (a, b, c, d, e, f, g, h) <- parseQuery q
    i                        <- parseQuery q
    return (a, b, c, d, e, f, g, h, i)

instance (FromQuery a, FromQuery b, FromQuery c, FromQuery d, FromQuery e, FromQuery f, FromQuery g, FromQuery h, FromQuery i, FromQuery j) => FromQuery (a, b, c, d, e, f, g, h, i, j) where
  parseQuery q = do
    (a, b, c, d, e, f, g, h, i) <- parseQuery q
    j                           <- parseQuery q
    return (a, b, c, d, e, f, g, h, i, j)

