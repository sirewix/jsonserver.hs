{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
#-}
module Main where
import           Auth
import           Control.Concurrent
import           Control.Monad
import           Data.Char
import           Data.Function
import           Database.PostgreSQL.Simple
import           Entry
import           Network.Wai.Handler.Warp       ( run )
import qualified Data.ByteString.Lazy          as B

main :: IO ()
main = do
  db <- connectPostgreSQL "host='localhost'"
  Prelude.putStrLn $ "http://localhost:8080/"
  keys    <- replicateM 2 generateSecret
  secrets <- newMVar keys
  void . forkIO $ do
    threadDelay $ 1000000 * 60 * 60
    updateSecrets secrets
  run 8080 (app secrets db)
