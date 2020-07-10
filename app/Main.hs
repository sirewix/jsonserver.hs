{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
#-}
module Main where
--import qualified Data.ByteString.Char8         as B
import           Control.Concurrent.MVar
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                , unfoldr
                                                )
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Database.PostgreSQL.Simple
                                         hiding ( Query )
import           Network.HTTP.Types             ( status200
                                                , status400
                                                , status500
                                                )
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Query
import           System.Random
import           System.Random
import           Web.JWT
import qualified Data.ByteString.Lazy          as B


-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
{-
app :: Application
app r respond = do
    putStrLn "I've done some IO here"
    print r
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"
-}
type App = (Response -> IO ResponseReceived) -> IO ResponseReceived

data AppResponse =
    Ok Text
  | BadRequest
  | InternalError
  | AccessDenied

type Secrets = MVar [Signer]

generateJWT :: Secrets -> Bool -> Text -> Text -> IO Text
generateJWT secrets admin name password = do
  now <- getPOSIXTime
  (secret:_) <- readMVar secrets
  let header = mempty { alg = Just HS256 }
      claims = mempty {
          iss = stringOrURI "thiswebserver"
        , sub = stringOrURI name
        , Web.JWT.exp = numericDate (now + 60 * 60)
        }
  return $ encodeSigned secret header claims

register (UserName name, LastName lastName, Password password) db = do
  dbres <- execute
    db
    "INSERT INTO users  (name, lastname, registrationdate, admin, password) VALUES (?, ?, current_timestamp, false, ?)"
    (name, lastName, password)
  return $ Ok ("Hellow " <> (pack . show $ dbres))

defaultDbHandlers =
    [ Handler (\(e :: FormatError) -> return InternalError)
    , Handler (\(e :: ResultError) -> return InternalError)
    , Handler (\(e :: SqlError) -> return InternalError) ]

login genToken (UserName name, Password password) db =
  flip catches (Handler (\(e :: QueryError) -> return AccessDenied) : defaultDbHandlers) $ do
    [Only admin] <- query
      db
      "SELECT admin FROM users WHERE name = ? AND password = ?"
      (name, password)
    token <- genToken admin name password
    return $ Ok token


newtype UserName = UserName Text
newtype LastName = LastName Text
newtype Password = Password Text

instance Query UserName where
  parseQuery q = UserName . decodeUtf8 <$> "name" .: q

instance Query LastName where
  parseQuery q = LastName . decodeUtf8 <$> "lastname" .: q

instance Query Password where
  parseQuery q = Password . decodeUtf8 <$> "password" .: q

app :: Secrets -> Connection -> Application
app secrets db req respond = do
  print req
  case pathInfo req of
    ["posts"   ] -> posts req respond
    ["register"] -> towai $ register
    ["login"   ] -> towai $ login $ generateJWT secrets
    _            -> respond
      $ responseLBS status400 [("Content-Type", "text/plain")] "Bad request"
 where
  towai :: Query q => (q -> Connection -> IO AppResponse) -> IO ResponseReceived
  towai f = case parseQuery (queryString req) of
    Just q -> f q db >>= \res -> case res of
      Ok txt ->
        respond $ responseLBS status200 [("Content-Type", "text/plain")] $ B.fromStrict $ encodeUtf8 txt
      BadRequest    -> badRequest
      InternalError -> respond $ responseLBS status500
                                             [("Content-Type", "text/plain")]
                                             "Internal server error"
    Nothing -> badRequest
  badRequest = respond $ responseLBS status400
                                     [("Content-Type", "text/plain")]
                                     "Bad request query"

posts :: Application
posts req respond =
  respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hellow"

generateSecret = hmacSecret . pack <$> replicateM 30 randomIO

updateSecrets secrets = modifyMVar_ secrets $ \secrets -> (: (init secrets)) <$> generateSecret

main :: IO ()
main = do
  db <- connectPostgreSQL "host='localhost'"
  Prelude.putStrLn $ "http://localhost:8080/"
  keys <- replicateM 2 generateSecret
  secrets <- newMVar keys
  void . forkIO $ do
      threadDelay $ 1000000 * 60 * 60
      updateSecrets secrets
  run 8080 (app secrets db)
