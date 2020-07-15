{-# LANGUAGE
  OverloadedStrings
#-}
module Query
  ( Query(..)
  , (.:)
  , (.:?)
  )
where

import           Data.ByteString
import           Control.Monad                  ( join )

type QueryParser a = [(ByteString, Maybe ByteString)] -> Maybe a

class Query a where
    parseQuery :: QueryParser a

(.:) :: ByteString -> QueryParser ByteString
q .: xs = join $ lookup q xs
(.:?) :: ByteString -> QueryParser (Maybe ByteString)
q .:? xs = lookup q xs

instance Query () where
    parseQuery _ = Just ()

instance Query a => Query (Maybe a) where
    parseQuery xs = Just <$> parseQuery xs

instance (Query a, Query b) => Query (a, b) where
  parseQuery q = do
    a <- parseQuery q
    b <- parseQuery q
    return (a, b)

instance (Query a, Query b, Query c) => Query (a, b, c) where
  parseQuery q = do
    a <- parseQuery q
    b <- parseQuery q
    c <- parseQuery q
    return (a, b, c)

instance (Query a, Query b, Query c, Query d) => Query (a, b, c, d) where
  parseQuery q = do
    a <- parseQuery q
    b <- parseQuery q
    c <- parseQuery q
    d <- parseQuery q
    return (a, b, c, d)

instance (Query a, Query b, Query c, Query d, Query e) => Query (a, b, c, d, e) where
  parseQuery q = do
    a <- parseQuery q
    b <- parseQuery q
    c <- parseQuery q
    d <- parseQuery q
    e <- parseQuery q
    return (a, b, c, d, e)

instance (Query a, Query b, Query c, Query d, Query e, Query f) => Query (a, b, c, d, e, f) where
  parseQuery q = do
    a <- parseQuery q
    b <- parseQuery q
    c <- parseQuery q
    d <- parseQuery q
    e <- parseQuery q
    f <- parseQuery q
    return (a, b, c, d, e, f)

instance (Query a, Query b, Query c, Query d, Query e, Query f, Query g) => Query (a, b, c, d, e, f, g) where
  parseQuery q = do
    a <- parseQuery q
    b <- parseQuery q
    c <- parseQuery q
    d <- parseQuery q
    e <- parseQuery q
    f <- parseQuery q
    g <- parseQuery q
    return (a, b, c, d, e, f, g)

instance (Query a, Query b, Query c, Query d, Query e, Query f, Query g, Query h) => Query (a, b, c, d, e, f, g, h) where
  parseQuery q = do
    a <- parseQuery q
    b <- parseQuery q
    c <- parseQuery q
    d <- parseQuery q
    e <- parseQuery q
    f <- parseQuery q
    g <- parseQuery q
    h <- parseQuery q
    return (a, b, c, d, e, f, g, h)

instance (Query a, Query b, Query c, Query d, Query e, Query f, Query g, Query h, Query i) => Query (a, b, c, d, e, f, g, h, i) where
  parseQuery q = do
    a <- parseQuery q
    b <- parseQuery q
    c <- parseQuery q
    d <- parseQuery q
    e <- parseQuery q
    f <- parseQuery q
    g <- parseQuery q
    h <- parseQuery q
    i <- parseQuery q
    return (a, b, c, d, e, f, g, h, i)

instance (Query a, Query b, Query c, Query d, Query e, Query f, Query g, Query h, Query i, Query j) => Query (a, b, c, d, e, f, g, h, i, j) where
  parseQuery q = do
    a <- parseQuery q
    b <- parseQuery q
    c <- parseQuery q
    d <- parseQuery q
    e <- parseQuery q
    f <- parseQuery q
    g <- parseQuery q
    h <- parseQuery q
    i <- parseQuery q
    j <- parseQuery q
    return (a, b, c, d, e, f, g, h, i, j)

