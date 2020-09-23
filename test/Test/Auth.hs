{-# LANGUAGE OverloadedStrings #-}

module Test.Auth (testAuth) where
import           App.Implementation.Auth        ( JWTVerification(..)
                                                , verifyJWT
                                                , generateJWT
                                                )
import           Data.Text                      ( pack )
import           Entities                       ( UserName(..) )
import           Test.Common                    ( signedCheck )
import           Test.QuickCheck                ( ASCIIString(..)
                                                , Positive(..)
                                                )
import           Web.JWT                        ( hmacSecret )

testAuth = do
  signedCheck "verification"            testVerification
  signedCheck "verification expiration" testExpiration
  signedCheck "verification claims"     testClaims

testVerification
  ( ASCIIString name
  , ASCIIString secret1
  , ASCIIString secret2
  ) = let secrets1 = [hmacSecret $ pack secret1]
          secrets2 = [hmacSecret $ pack secret2]
          jwt = generateJWT 10 (False, False, pack name) (1, secrets1)
       in verifyJWT Nothing jwt (1, secrets2)
          == if secret1 == secret2
                then JWTOk . UserName $ pack name
                else JWTReject

testExpiration
  ( admin
  , author
  , ASCIIString name
  , ASCIIString secret
  , Positive now
  , Positive timePassed
  , Positive expTime
  ) = let secrets = [hmacSecret $ pack secret]
          jwt = generateJWT (fromInteger expTime) (admin, author, pack name) (fromInteger now, secrets)
          need = pack <$>
                      if admin  then Just "admin"
                 else if author then Just "author"
                 else Nothing
       in verifyJWT need jwt (fromInteger (now + timePassed), secrets)
          == if timePassed < expTime
                then JWTOk . UserName $ pack name
                else JWTExp

testClaims
  ( admin
  , author
  , ASCIIString name
  , ASCIIString secret
  ) = let secrets = [hmacSecret $ pack secret]
          jwt = generateJWT 10 (not admin, not author, pack name) (1, secrets)
          need = pack <$>
                    if admin  then Just "admin"
               else if author then Just "author"
               else Nothing
       in verifyJWT need jwt (1, secrets)
          == if admin || author
                then JWTReject
                else JWTOk . UserName $ pack name

