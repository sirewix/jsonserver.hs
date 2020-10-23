{-# LANGUAGE OverloadedStrings #-}

module Test.Auth (testAuth) where
import           App.Implementation.Auth        ( JWTVerification(..)
                                                , verifyJWT
                                                , generateJWT
                                                )
import           Data.Text                      ( pack )
import           Test.Common                    ( signedCheck )
import           Test.QuickCheck                ( ASCIIString(..)
                                                , Positive(..)
                                                )
import           Web.JWT                        ( hmacSecret )

testAuth :: IO ()
testAuth = do
  signedCheck "verification"            testVerification
  signedCheck "verification expiration" testExpiration
  signedCheck "verification claims"     testClaims

testVerification
  :: ( ASCIIString
     , ASCIIString
     , ASCIIString
     )
  -> Bool
testVerification
  ( ASCIIString name
  , ASCIIString secret1
  , ASCIIString secret2
  ) = let secrets1 = [hmacSecret $ pack secret1]
          secrets2 = [hmacSecret $ pack secret2]
          jwt = generateJWT (False, False, pack name) (10, 1, secrets1)
       in verifyJWT Nothing jwt (10, 1, secrets2)
          == if secret1 == secret2
                then JWTOk $ pack name
                else JWTReject

testExpiration
  :: ( Bool
     , Bool
     , ASCIIString
     , ASCIIString
     , Positive Integer
     , Positive Integer
     , Positive Integer
     )
  -> Bool
testExpiration
  ( admin
  , author
  , ASCIIString name
  , ASCIIString secret
  , Positive now
  , Positive timePassed
  , Positive expTime
  ) = let secrets = [hmacSecret $ pack secret]
          jwt = generateJWT  (admin, author, pack name) (fromInteger expTime, fromInteger now, secrets)
          need = pack <$>
                      if admin  then Just "admin"
                 else if author then Just "author"
                 else Nothing
       in verifyJWT need jwt (fromInteger expTime, fromInteger (now + timePassed), secrets)
          == if timePassed < expTime
                then JWTOk $ pack name
                else JWTExp

testClaims
  :: ( Bool
     , Bool
     , ASCIIString
     , ASCIIString
     )
  -> Bool
testClaims
  ( admin
  , author
  , ASCIIString name
  , ASCIIString secret
  ) = let secrets = [hmacSecret $ pack secret]
          jwt = generateJWT (not admin, not author, pack name) (10, 1, secrets)
          need = pack <$>
                    if admin  then Just "admin"
               else if author then Just "author"
               else Nothing
       in verifyJWT need jwt (1, 1, secrets)
          == if admin || author
                then JWTReject
                else JWTOk $ pack name

