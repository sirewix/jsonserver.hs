module App.Implementation.Auth
  ( Secrets
  , JWTVerification(..)
  , generateSecret
  , updateSecrets
  , generateJWT
  , verifyJWT
  , runJWT
  )
where

import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Auth             ( JWTVerification(..)
                                                , Secrets
                                                )
import           Config                         ( Config(..) )
import           Control.Concurrent.MVar        ( readMVar
                                                , modifyMVar_
                                                )
import           Control.Monad                  ( replicateM )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( (.=) )
import           Data.Foldable                  ( asum )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Time.Clock                ( NominalDiffTime )
import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           System.Random                  ( randomIO )
import           Web.JWT                        ( Algorithm(..)
                                                , ClaimsMap(..)
                                                , JOSEHeader(..)
                                                , JWTClaimsSet(..)
                                                , Signer
                                                , claims
                                                , decodeAndVerifySignature
                                                , encodeSigned
                                                , hmacSecret
                                                , numericDate
                                                , secondsSinceEpoch
                                                , stringOrURI
                                                , stringOrURIToText
                                                )
import qualified Data.Aeson                    as J
import qualified Data.Aeson.Types              as J
import qualified Data.Map.Strict               as Map
import qualified Web.JWT                       as JWT

runJWT :: (HasEnv Secrets m, HasEnv Config m, MonadIO m) => m (Integer, NominalDiffTime, [Signer])
runJWT = do
  secrets <- getEnv @Secrets
  expTime <- (60 *) . secrets_update_interval <$> getEnv @Config
  now  <- liftIO getPOSIXTime
  keys <- liftIO $ readMVar secrets
  return (expTime, now, keys)

generateJWT :: (Bool, Bool, Text) -> (Integer, NominalDiffTime, [Signer]) -> Text
generateJWT (_, _, _) (_, _, []) = error "secrets list must not be empty"
generateJWT (admin, author, name) (expTime, now, secret : _) =
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

verifyJWT :: Maybe Text -> Text -> (Integer, NominalDiffTime, [Signer]) -> JWTVerification Text
verifyJWT need jwt (_, now, keys) = do
  verified <- mb . asum . flip map keys $ \k -> decodeAndVerifySignature k jwt
  let cs = claims verified
  expires <- mb $ JWT.exp cs
  name    <- mb $ stringOrURIToText <$> JWT.sub cs
  auth    <- mb $ maybe (Just True) (lc cs) need
  if not auth
    then JWTReject
  else if now < secondsSinceEpoch expires
    then JWTOk name
    else JWTExp
 where
  lc cs c = J.parseMaybe J.parseJSON =<< Map.lookup c (unClaimsMap $ unregisteredClaims cs)
  mb = maybe JWTReject JWTOk

generateSecret :: IO Signer
generateSecret = hmacSecret . pack <$> replicateM 30 randomIO

updateSecrets :: Secrets -> IO ()
updateSecrets secrets = modifyMVar_ secrets $ \secrets -> (: init secrets) <$> generateSecret
