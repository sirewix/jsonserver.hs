{-# LANGUAGE QuasiQuotes #-}

module API.Users where

import           App.Response                   ( AppResponse(..) )
import           App.Prototype.App              ( HasEnv(..) )
import           App.Prototype.Auth             ( Admin(..)
                                                , Secrets )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
import           App.Prototype.Database         ( DbAccess
                                                , paginate
                                                )
import           Config                         ( Config(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Char                      ( isAlphaNum )
import           Data.Text                      ( Text )
import           Misc                           ( readT )
import           Query.Common                   ( Page(..) )
import           Query.FromQuery                ( FromQuery(..)
                                                , QueryParser
                                                , param
                                                , opt
                                                , filterQuery
                                                , liftMaybe
                                                )
import qualified Model.Users                   as M
import qualified Data.Text                     as T
import qualified Data.Aeson                    as J

newtype UserName = UserName T.Text

instance FromQuery UserName where
  parseQuery = UserName <$> parseUserName

parseUserName :: QueryParser Text
parseUserName =
    filterQuery (T.all isAlphaNum)
  . filterQuery ((\l -> l >= 3 && l <= 20) . T.length)
  $ param "username"

parsePassword :: QueryParser Text
parsePassword =
    filterQuery ((\l -> l >= 3 && l <= 30) . T.length)
  $ param "password"

getUsers (Page page) = AppOk . paginate <$> M.getUsers page

deleteUser
  :: (HasLog m, DbAccess m)
  => Admin
  -> UserName
  -> m AppResponse
deleteUser (Admin admin) (UserName user) = do
  res <- M.deleteUser user
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ admin <> " deleted user " <> user
      return . AppOk $ J.Null

data Register = Register
  { name     :: Text
  , lastname :: Text
  , password :: Text
  , avatar   :: Maybe Text
  }

instance FromQuery Register where
  parseQuery =
    Register
      <$> parseUserName
      <*> filterQuery ((<= 30) . T.length) (param "lastname")
      <*> parsePassword
      <*> (liftMaybe . maybe (Just Nothing) (fmap Just . readT) =<< opt "avatar")

register
  :: (HasLog m, DbAccess m)
  => Register
  -> m AppResponse
register (Register {..}) = do
  let entity = M.UserEssential
        { M.name     = name
        , M.lastname = lastname
        , M.admin    = False
        , M.avatar   = avatar
        }
  res <- M.createUser entity password
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ "new user" <> name <> " " <> lastname
      return (AppOk J.Null)

newtype Credentials = Credentials M.Credentials

instance FromQuery Credentials where
  parseQuery = fmap Credentials . M.Credentials
    <$> parseUserName
    <*> parsePassword

login
  :: ( HasLog m
     , DbAccess m
     , HasEnv Secrets m
     , HasEnv Config m
     , MonadIO m
     )
  => Credentials
  -> m AppResponse
login (Credentials creds@(M.Credentials {..})) = do
  res <- M.getUserToken creds
  case res of
    Left _ -> return AccessDenied
    Right token -> do
      log' Info $ name <> " logged in"
      return . AppOk $ J.String token
