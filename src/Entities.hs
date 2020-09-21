{-# LANGUAGE
    OverloadedStrings
  , DeriveAnyClass
  , DeriveGeneric
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module Entities where

import           Control.Arrow                  ( first )
import           Data.Aeson                     ( ToJSON )
import           Data.Char                      ( isAlphaNum )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Time.Calendar             ( showGregorian )
import           Database.PostgreSQL.Simple     ( FromRow )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..) )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..) )
import           GHC.Generics                   ( Generic )
import           Misc                           ( readT
                                                , filterMaybe
                                                )
import           FromQuery                          ( (.:)
                                                , FromQuery(..)
                                                )
import qualified Data.Aeson                    as J
import qualified Data.Text                     as T
import qualified Database.PostgreSQL.Simple.Time
                                               as T

newtype UserName = UserName Text deriving (Eq, Show)
newtype LastName = LastName Text
newtype Password = Password Text

instance FromQuery UserName where
  parseQuery q = fmap UserName
    . filterMaybe (T.all isAlphaNum)
    . filterMaybe ((\l -> l >= 3 && l <= 20) . T.length)
    $ ("username" .: q)

instance FromQuery LastName where
  parseQuery q = fmap LastName
    . filterMaybe (T.all isAlphaNum)
    . filterMaybe ((<= 30) . T.length)
    $ "lastname" .: q

instance FromQuery Password where
  parseQuery q = fmap Password
    . filterMaybe ((\l -> l >= 6 && l <= 30) . T.length)
    $ "password" .: q

newtype Description = Description Text
instance FromQuery Description where
  parseQuery q = Description <$> "description" .: q

newtype Token = Token Text
instance FromQuery Token where
  parseQuery q = Token <$> "token" .: q

newtype Tag = Tag Int
    deriving (Generic, FromRow, ToJSON)

instance FromQuery Tag where
  parseQuery q = Tag <$> (readT =<< "tag" .: q)

newtype CategoryId = CategoryId { unCategoryId :: Int }

instance ToField CategoryId where
  toField (CategoryId cid) = toField cid

instance FromQuery CategoryId where
  parseQuery q = CategoryId <$> (readT =<< "cid" .: q)

newtype Name = Name Text
instance FromQuery Name where
  parseQuery q = Name <$> "name" .: q

newtype Date = Date { unDate :: T.Date }

instance ToField Entities.Date where
  toField (Date d) = toField d

instance Read Entities.Date where
  readsPrec d = map (first Date) . readsPrec d

instance ToJSON Entities.Date where
  toJSON (Date (T.Finite d)) = J.String $ pack $ showGregorian d
  toJSON _                 = J.Null

newtype AuthorName = AuthorName { unAuthorName :: Text }
instance FromQuery AuthorName where
  parseQuery q = fmap AuthorName
    . filterMaybe (T.all isAlphaNum)
    . filterMaybe ((\l -> l >= 3 && l <= 20) . T.length)
    $ ("author_name" .: q)

newtype Title = Title { unTitle :: Text }
instance FromQuery Title where
  parseQuery q = fmap Title
    . filterMaybe ((<= 50) . T.length)
    . filterMaybe (not . T.null)
    $ "title" .: q

instance ToField Title where
  toField (Title imgs) = toField imgs

newtype Search = Search { unSearch :: Text }
instance FromQuery Search where
  parseQuery q = fmap Search
    . filterMaybe (not . T.null)
    $ "search" .: q

newtype Content = Content { unContent :: Text }
instance FromQuery Content where
  parseQuery q = fmap Content
    . filterMaybe (not . T.null)
    $ "text" .: q

instance ToField Content where
  toField (Content imgs) = toField imgs

newtype Image = Image Text
instance FromQuery Image where
  parseQuery q = fmap Image
    . filterMaybe (not . T.null)
    $ "image" .: q

instance ToField Image where
  toField (Image imgs) = toField imgs

newtype Images = Images [Text]
instance FromQuery Images where
  parseQuery q = fmap Images
    . filterMaybe (not . any T.null)
    $ (readT =<< "images" .: q)

newtype PostId = PostId Int

instance FromQuery PostId where
  parseQuery q = PostId <$> (readT =<< "pid" .: q)

instance ToField Images where
  toField (Images imgs) = toField $ PGArray imgs

newtype Page = Page Int
instance FromQuery Page where
  parseQuery q = Page <$> maybe (Just 1) readT ("page" .: q)
