module App.Prototype.Auth
  ( Secrets
  , JWTVerification(..)
  )
where

import           Control.Concurrent.MVar        ( MVar )
import           Web.JWT                        ( Signer )

type Secrets = MVar [Signer]

data JWTVerification a =
    JWTOk a
  | JWTExp
  | JWTReject
  deriving (Eq, Show)

instance Functor JWTVerification where
  fmap f (JWTOk a) = JWTOk (f a)
  fmap _ JWTExp    = JWTExp
  fmap _ JWTReject = JWTReject

instance Applicative JWTVerification where
  pure = JWTOk
  (JWTOk f) <*> (JWTOk a) = JWTOk (f a)
  (JWTOk _) <*> JWTExp    = JWTExp
  (JWTOk _) <*> JWTReject = JWTReject
  JWTExp    <*> _         = JWTExp
  JWTReject <*> _         = JWTReject

instance Monad JWTVerification where
  (JWTOk a) >>= f = f a
  JWTExp    >>= _ = JWTExp
  JWTReject >>= _ = JWTReject
