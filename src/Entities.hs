{-# LANGUAGE
    DuplicateRecordFields
  , OverloadedStrings
  , DeriveAnyClass
  , DeriveGeneric
  , TypeSynonymInstances
  , FlexibleInstances
  , TypeOperators
  #-}
module Entities where
import           Control.Arrow                  ( first )
import           Data.Aeson                     ( ToJSON
                                                , (.=)
                                                )
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
import           Query                          ( (.:)
                                                , Query(..)
                                                )
import qualified Data.Aeson                    as J
import qualified Data.Text                     as T
import qualified Database.PostgreSQL.Simple.Time
                                               as T

newtype UserName = UserName Text deriving (Eq, Show)
newtype LastName = LastName Text
newtype Password = Password Text

instance Query UserName where
  parseQuery q = fmap UserName
    . filterMaybe (T.all isAlphaNum)
    . filterMaybe ((\l -> l >= 3 && l <= 20) . T.length)
    $ ("username" .: q)

instance Query LastName where
  parseQuery q = fmap LastName
    . filterMaybe (T.all isAlphaNum)
    . filterMaybe ((<= 30) . T.length)
    $ "lastname" .: q

instance Query Password where
  parseQuery q = fmap Password
    . filterMaybe ((\l -> l >= 6 && l <= 30) . T.length)
    $ "password" .: q

newtype Description = Description Text
instance Query Description where
  parseQuery q = Description <$> "description" .: q

newtype Token = Token Text
instance Query Token where
  parseQuery q = Token <$> "token" .: q

data Author = Author
  { _id         :: Int
  , username    :: Text
  , description :: Text
  } deriving (Generic, FromRow, ToJSON)

newtype Tag = Tag Int
    deriving (Generic, FromRow, ToJSON)

instance Query Tag where
  parseQuery q = Tag <$> (readT =<< "tag" .: q)

data Category = Category Int Text
    deriving (Generic, FromRow)

instance ToJSON Category where
  toJSON (Category id name) = J.object ["cid" .= id, "name" .= name]

newtype CategoryId = CategoryId Int

instance ToField CategoryId where
  toField (CategoryId cid) = toField cid

instance Query CategoryId where
  parseQuery q = CategoryId <$> (readT =<< "cid" .: q)

newtype Name = Name Text
instance Query Name where
  parseQuery q = Name <$> "name" .: q

newtype Date = Date { unDate :: T.Date }

instance ToField Entities.Date where
  toField (Date d) = toField d

instance Read Entities.Date where
  readsPrec d = map (first Date) . readsPrec d

instance ToJSON Entities.Date where
  toJSON (Date (T.Finite d)) = J.String $ pack $ showGregorian d
  toJSON _                 = J.Null

newtype AuthorName = AuthorName Text
instance Query AuthorName where
  parseQuery q = fmap AuthorName
    . filterMaybe (T.all isAlphaNum)
    . filterMaybe ((\l -> l >= 3 && l <= 20) . T.length)
    $ ("author_name" .: q)

newtype Title = Title Text
instance Query Title where
  parseQuery q = fmap Title
    . filterMaybe ((<= 50) . T.length)
    . filterMaybe (not . T.null)
    $ "title" .: q

instance ToField Title where
  toField (Title imgs) = toField imgs

newtype Search = Search Text
instance Query Search where
  parseQuery q = fmap Search
    . filterMaybe (not . T.null)
    $ "search" .: q

newtype Content = Content Text
instance Query Content where
  parseQuery q = fmap Content
    . filterMaybe (not . T.null)
    $ "text" .: q

instance ToField Content where
  toField (Content imgs) = toField imgs

newtype Image = Image Text
instance Query Image where
  parseQuery q = fmap Image
    . filterMaybe (not . T.null)
    $ "image" .: q

instance ToField Image where
  toField (Image imgs) = toField imgs

newtype Images = Images [Text]
instance Query Images where
  parseQuery q = fmap Images
    . filterMaybe (not . any T.null)
    $ (readT =<< "images" .: q)

newtype PostId = PostId Int

instance Query PostId where
  parseQuery q = PostId <$> (readT =<< "pid" .: q)

instance ToField Images where
  toField (Images imgs) = toField $ PGArray imgs

newtype Page = Page Int
instance Query Page where
  parseQuery q = Page <$> maybe (Just 1) readT ("page" .: q)
