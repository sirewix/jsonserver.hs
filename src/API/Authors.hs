{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  #-}

module API.Authors where

import           API.Users                      ( UserName(..) )
import           App.Response                   ( AppResponse(..) )
import           App.Prototype.App              ( HasEnv )
import           App.Prototype.Database         ( DbAccess(..), paginate )
import           App.Prototype.Log              ( HasLog(..)
                                                , Priority(..)
                                                )
import           App.Prototype.Auth             ( Admin(..) )
import           Config                         ( Config )
import           Data.Text                      ( Text )
import           Query.Common                   ( Page(..) )
import           Query.FromQuery                ( FromQuery(..)
                                                , param
                                                , opt
                                                )
import qualified Data.Aeson                    as J
import qualified Model.Authors                 as M

getAuthors
  :: ( Monad m
     , DbAccess m
     , HasEnv Config m
     )
  => Admin
  -> Page
  -> m AppResponse
getAuthors (Admin _) (Page page) = AppOk . paginate <$> M.getAuthors page

newtype AuthorEssential = AuthorEssential M.AuthorEssential

instance FromQuery AuthorEssential where
  parseQuery = fmap AuthorEssential . M.AuthorEssential
    <$> param "username"
    <*> param "description"

newtype AuthorPartial = AuthorPartial M.AuthorPartial

instance FromQuery AuthorPartial where
  parseQuery = fmap AuthorPartial . M.AuthorPartial
    <$> opt "username"
    <*> opt "description"

makeAuthor (Admin admin) (AuthorEssential entity@M.AuthorEssential {..}) = do
  res <- M.makeAuthor entity
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ admin <> " promoted " <> username <> " to authors with description \"" <> description <> "\""
      return (AppOk J.Null)

newtype PreviousUserName = PreviousUserName Text

instance FromQuery PreviousUserName where
  parseQuery = PreviousUserName <$> param "previous_username"

editAuthor (Admin admin) (PreviousUserName prevName, AuthorPartial entity@M.AuthorPartial {..}) = do
  res <- M.editAuthor prevName entity
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ admin <> " edited author " <> prevName
      return (AppOk J.Null)

deleteAuthor (Admin admin) (UserName name) = do
  res <- M.deleteAuthor name
  case res of
    Left _ -> return BadRequest
    Right () -> do
      log' Info $ admin <> " exiled " <> name <> " from authors guild"
      return (AppOk J.Null)
