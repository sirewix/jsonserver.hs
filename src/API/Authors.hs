{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , QuasiQuotes
  #-}

module API.Authors where

import           API.Users                      ( UserName(..) )
import           App.Response                   ( AppResponse(..) )
import           App.Prototype.App              ( HasEnv )
import           App.Prototype.Log              ( HasLog(..) )
import           App.Prototype.Database         ( DbAccess(..)
                                                , paginate
                                                , unwrapRequest
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

makeAuthor
  :: (DbAccess m, HasLog m)
  => Admin
  -> AuthorEssential
  -> m AppResponse
makeAuthor (Admin admin) (AuthorEssential entity@M.AuthorEssential {..}) = do
  M.makeAuthor entity >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ admin <> " promoted " <> username
      <> " to authors with description \"" <> description <> "\"")

newtype PreviousUserName = PreviousUserName Text

instance FromQuery PreviousUserName where
  parseQuery = PreviousUserName <$> param "previous_username"

editAuthor
  :: (DbAccess m, HasLog m)
  => Admin
  -> (PreviousUserName, AuthorPartial)
  -> m AppResponse
editAuthor (Admin admin) (PreviousUserName prevName, AuthorPartial entity@M.AuthorPartial {..}) = do
  M.editAuthor prevName entity >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ admin <> " edited author " <> prevName)

deleteAuthor
  :: (DbAccess m, HasLog m)
  => Admin
  -> UserName
  -> m AppResponse
deleteAuthor (Admin admin) (UserName name) = do
  M.deleteAuthor name >>= unwrapRequest BadRequest
    (const J.Null)
    (Just . const $ admin <> " exiled " <> name <> " from authors guild")
