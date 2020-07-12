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
                                                , status401
                                                , status500
                                                , Status(..)
                                                )
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Query
import           System.Random
import           System.Random
import           Web.JWT
import qualified Web.JWT as JWT
import qualified Data.ByteString.Lazy          as B
import qualified Data.Aeson                    as J
import qualified Data.Aeson.Types              as J
import           Data.Aeson ((.=))
import qualified Data.Map.Strict as Map
import           Data.Foldable

type App = (Response -> IO ResponseReceived) -> IO ResponseReceived

data AppResponse =
    AppOk J.Value
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
          sub = stringOrURI name
        , JWT.exp = numericDate (now + 60 * 60)
        , unregisteredClaims = ClaimsMap $ Map.fromList
            [ "admin" .= admin
            , "author" .= False ]
        }
  return $ encodeSigned secret header claims

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

verifyJWT :: Secrets -> Text -> IO (JWTVerification (Text, Bool, Bool))
verifyJWT secrets jwt = do
  now  <- getPOSIXTime
  keys <- readMVar secrets
  return $ do
    verified <- mb . asum . flip map keys $ \k -> decodeAndVerifySignature k jwt
    let cs = claims verified
    expires <- mb $ JWT.exp cs
    if secondsSinceEpoch expires < now
      then JWTExp
      else mb $ do
        name   <- JWT.sub cs
        admin  <- lc "admin" cs
        author <- lc "author" cs
        return (stringOrURIToText name, admin, author)
 where
  lc c cs = J.parseMaybe J.parseJSON
    =<< Map.lookup c (unClaimsMap $ unregisteredClaims cs)
  mb = maybe JWTReject JWTOk

register (UserName name, LastName lastName, Password password) db = do
  dbres <- execute
    db
    "INSERT INTO users  (name, lastname, registrationdate, admin, password) VALUES (?, ?, current_timestamp, false, ?)"
    (name, lastName, password)
  return $ AppOk $ J.Bool True

defaultDbHandlers =
    [ Handler (\(e :: FormatError) -> return InternalError)
    , Handler (\(e :: ResultError) -> return InternalError)
    , Handler (\(e :: SqlError) -> return InternalError) ]

login genToken (UserName name, Password password) db =
  flip catches (Handler (\(e :: QueryError) -> return AccessDenied) : defaultDbHandlers) $ do
    q <- query
      db
      "SELECT admin FROM users WHERE name = ? AND password = ?"
      (name, password)
    case q of
      [Only admin] -> do
        print admin
        token <- genToken admin name password
        return . AppOk $ J.String token
      _ -> return AccessDenied

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
    ["register"] -> towai register
    ["login"   ] -> towai . login $ generateJWT secrets
    _            -> respond $ err status400
 where
  towai :: Query q => (q -> Connection -> IO AppResponse) -> IO ResponseReceived
  towai f = respond =<< case parseQuery (queryString req) of
    Just q -> f q db >>= \res -> return $ case res of
      AppOk txt -> ok txt
      BadRequest    -> err status400
      InternalError -> err status500
      AccessDenied  -> err status401
    Nothing -> return $ err status400
  json status x =
    responseLBS status [("Content-Type", "application/json")]
      . J.encode
      . J.object
      $ x
  ok x = json status200 $
      [ "ok" .= True
      , "response" .= x ]
  err status = json status $
      [ "ok" .= False
      , "code" .= statusCode status
      , "error" .= (decodeUtf8 $ statusMessage status) ]

posts :: Application
posts req respond = undefined

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
