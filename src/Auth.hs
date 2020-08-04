{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}
module Auth
  ( Secrets
  , JWTVerification(..)
  , login
  , register
  , generateSecret
  , updateSecrets
  , generateJWT
  , verifyJWT
  , runJWT
  )
where

import           App
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Aeson                     ( (.=) )
import           Data.Foldable
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Database.PostgreSQL.Simple
import           Logger
import           Entities
import           System.Random
import           Web.JWT
import qualified Data.Aeson                    as J
import qualified Data.Aeson.Types              as J
import qualified Data.Map.Strict               as Map
import qualified Web.JWT                       as JWT

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


generateJWT :: Integer -> (Bool, Bool, Text) -> (NominalDiffTime, [Signer]) -> Text
generateJWT _ (_, _, _) (_, []) = error "secrets list must not be empty"
generateJWT expTime (admin, author, name) (now, secret : _) =
  let
    header = mempty { alg = Just HS256 }
    claims = mempty
      { sub                = stringOrURI name
      , JWT.exp            = numericDate (now + fromInteger expTime)
      , unregisteredClaims = ClaimsMap
                               $ Map.fromList ["admin" .= admin, "author" .= author]
      }
  in
    encodeSigned secret header claims

runJWT :: Secrets -> IO (NominalDiffTime, [Signer])
runJWT secrets = do
  now  <- getPOSIXTime
  keys <- readMVar secrets
  return (now, keys)

verifyJWT :: Maybe Text -> Text -> (NominalDiffTime, [Signer]) -> JWTVerification UserName
verifyJWT need jwt (now, keys) = do
  verified <- mb . asum . flip map keys $ \k -> decodeAndVerifySignature k jwt
  let cs = claims verified
  expires <- mb $ JWT.exp cs
  name    <- mb $ UserName . stringOrURIToText <$> JWT.sub cs
  auth    <- mb $ maybe (Just True) (lc cs) need
  if not auth
    then JWTReject
  else if now < secondsSinceEpoch expires
    then JWTOk name
    else JWTExp
 where
  lc cs c = J.parseMaybe J.parseJSON =<< Map.lookup c (unClaimsMap $ unregisteredClaims cs)
  mb = maybe JWTReject JWTOk

register (UserName name, LastName lastName, Password password) = execdb
  "INSERT INTO users (name, lastname, registrationdate, admin, password) VALUES (?, ?, current_timestamp, false, ?)"
  (name, lastName, password)
  (Just $ "new user" <> name <> " " <> lastName)

login genToken (UserName name, Password password) (log, db) = catchDb log (return AccessDenied) $ do
  q <- query db "SELECT admin FROM users WHERE name = ? AND password = ?" (name, password)
  case q of
    [Only admin] -> do
      token <- genToken (admin, False, name)
      _ <- log Info $ name <> " logged in"
      return . AppOk $ J.String token
    _ -> return AccessDenied

generateSecret = hmacSecret . pack <$> replicateM 30 randomIO

updateSecrets secrets = modifyMVar_ secrets $ \secrets -> (: init secrets) <$> generateSecret
