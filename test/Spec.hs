{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Auth
import Entities
import Web.JWT
import           Data.Text                      ( pack )

main :: IO ()
main = do
  putStr "Testing verification\n"
  quickCheck $ \ ( admin
                 , author
                 , ASCIIString name
                 , ASCIIString secret
                 , Positive now
                 , Positive timePassed
                 , Positive expTime
                 ) ->
    let secrets = [hmacSecret $ pack secret]
        jwt = generateJWT (fromInteger expTime) (admin, author, pack name) (fromInteger now, secrets)
        need = pack <$>
                    if admin  then Just "admin"
               else if author then Just "author"
               else Nothing
     in verifyJWT need jwt (fromInteger (now + timePassed), secrets)
        == if timePassed < expTime
              then JWTOk . UserName $ pack name
              else JWTExp

  quickCheck $ \ ( admin
               , author
               , ASCIIString name
               , ASCIIString secret
               ) ->
    let secrets = [hmacSecret $ pack secret]
        jwt = generateJWT 10 (not admin, not author, pack name) (1, secrets)
        need = pack <$>
                    if admin  then Just "admin"
               else if author then Just "author"
               else Nothing
     in verifyJWT need jwt (1, secrets)
        == if admin || author
              then JWTReject
              else JWTOk . UserName $ pack name
