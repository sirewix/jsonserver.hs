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
    ) where

import           App
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Data.Aeson                     ( (.=) )
import           Data.Foldable
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Database.PostgreSQL.Simple
                                         hiding ( Query )
import           Query
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

instance Functor JWTVerification where
  fmap f (JWTOk a) = JWTOk (f a)
  fmap f JWTExp    = JWTExp
  fmap f JWTReject = JWTReject

instance Applicative JWTVerification where
  pure = JWTOk
  (JWTOk f) <*> (JWTOk a) = JWTOk (f a)
  JWTExp    <*> _         = JWTExp
  JWTReject <*> _         = JWTReject

instance Monad JWTVerification where
  (JWTOk a) >>= f = f a
  JWTExp    >>= _ = JWTExp
  JWTReject >>= _ = JWTReject


generateJWT :: (Bool, Bool, Text, Text) -> (NominalDiffTime, [Signer]) -> Text
generateJWT (admin, author, name, password) (now, (secret:_))  =
  let header = mempty { alg = Just HS256 }
      claims = mempty {
          sub = stringOrURI name
        , JWT.exp = numericDate (now + 60 * 60)
        , unregisteredClaims = ClaimsMap $ Map.fromList
            [ "admin" .= admin
            , "author" .= author ]
        }
   in encodeSigned secret header claims

runJWT :: Secrets -> IO (NominalDiffTime, [Signer])
runJWT secrets = do
  now  <- getPOSIXTime
  keys <- readMVar secrets
  return (now, keys)

verifyJWT
  :: Maybe Text
  -> Text
  -> (NominalDiffTime, [Signer])
  -> JWTVerification UserName
verifyJWT need jwt (now, keys) = do
  verified <- mb . asum . flip map keys $ \k -> decodeAndVerifySignature k jwt
  let cs = claims verified
  expires <- mb $ JWT.exp cs
  name    <- mb $ UserName . stringOrURIToText <$> JWT.sub cs
  auth    <- mb $ maybe (Just True) (lc cs) need
  if not auth
    then JWTReject
    else if secondsSinceEpoch expires < now then JWTExp else JWTOk name
 where
  lc cs c = J.parseMaybe J.parseJSON
    =<< Map.lookup c (unClaimsMap $ unregisteredClaims cs)
  mb = maybe JWTReject JWTOk

register (UserName name, LastName lastName, Password password) (log, db) = do
  dbres <- execute
    db
    "INSERT INTO users (name, lastname, registrationdate, admin, password) VALUES (?, ?, current_timestamp, false, ?)"
    (name, lastName, password)
  log Info $ "new user" <> name <> " " <> lastName
  return $ AppOk $ J.Bool True

login genToken (UserName name, Password password) (log, db) =
  flip catches (Handler (\(e :: QueryError) -> return AccessDenied) : defaultDbHandlers log) $ do
    q <- query
      db
      "SELECT admin FROM users WHERE name = ? AND password = ?"
      (name, password)
    case q of
      [Only admin] -> do
        token <- genToken (admin, False, name, password)
        log Info $ name <> " logged in"
        return . AppOk $ J.String token
      _ -> return AccessDenied

generateSecret = hmacSecret . pack <$> replicateM 30 randomIO

updateSecrets secrets = modifyMVar_ secrets $ \secrets -> (: (init secrets)) <$> generateSecret
