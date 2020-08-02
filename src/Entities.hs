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
import           Data.Aeson (ToJSON,(.=))
import           Data.Text                      ( Text, pack, unpack)
import           Data.Text.Encoding
import           Data.Time.Calendar
import           Database.PostgreSQL.Simple hiding (Query)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Time
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Types (PGArray(..))
import           GHC.Generics
import           Misc
import           Query
import qualified Data.Aeson as J

newtype UserName = UserName Text deriving (Eq, Show)
newtype LastName = LastName Text
newtype Password = Password Text

instance Query UserName where
  parseQuery q = UserName <$> "username" .: q

instance Query LastName where
  parseQuery q = LastName <$> "lastname" .: q

instance Query Password where
  parseQuery q = Password <$> "password" .: q

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
    toJSON (Category id name) = J.object
        [ "cid" .= id
        , "name" .= name ]

newtype CategoryId = CategoryId Int

instance ToField CategoryId where
    toField (CategoryId cid) = toField cid

instance Query CategoryId where
  parseQuery q = CategoryId <$> (readT =<< "cid" .: q)

newtype Name = Name Text
instance Query Name where
  parseQuery q = Name <$> "name" .: q

instance ToJSON Date where
    toJSON (Finite d) = J.String $ pack $ showGregorian d
    toJSON _ = J.Null

newtype AuthorName = AuthorName Text
instance Query AuthorName where
  parseQuery q = AuthorName <$> "author_name" .: q

newtype Title = Title Text
instance Query Title where
  parseQuery q = Title <$> "title" .: q
instance ToField Title where
  toField (Title imgs) = toField imgs

newtype Search = Search Text
instance Query Search where
  parseQuery q = Search <$> "search" .: q

newtype Content = Content Text
instance Query Content where
  parseQuery q = Content <$> "text" .: q
instance ToField Content where
  toField (Content imgs) = toField imgs

newtype Image = Image Text
instance Query Image where
  parseQuery q = Image <$> "image" .: q
instance ToField Image where
  toField (Image imgs) = toField imgs

newtype Images = Images [Text]
instance Query Images where
  parseQuery q = Images <$> (readT =<< "images" .: q)

newtype PostId = PostId Int

instance Query PostId where
  parseQuery q = PostId <$> (readT =<< "pid" .: q)

instance ToField Images where
  toField (Images imgs) = toField $ PGArray imgs

newtype Page = Page Int
instance Query Page where
  parseQuery q = Page <$> maybe (Just 1) readT ("page" .: q)
