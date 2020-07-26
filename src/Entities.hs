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
import qualified Data.Aeson as J
import           Data.Aeson (ToJSON,(.=))
import           Data.Text                      ( Text, pack, unpack)
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple hiding (Query)
import           GHC.Generics
import           Query
import           Data.Maybe
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Time
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types (PGArray(..))
import           Data.Time.Calendar
import           Data.Time.Calendar

newtype UserName = UserName Text
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

newtype Tag = Tag Text
    deriving (Generic, FromRow, ToJSON)

instance Query Tag where
  parseQuery q = Tag <$> "tag" .: q

data Category = Category Int Text
    deriving (Generic, FromRow)

instance ToJSON Category where
    toJSON (Category id name) = J.object
        [ "cid" .= id
        , "name" .= name ]

newtype CategoryId = CategoryId Int
    --deriving (Generic, ToField)

instance ToField CategoryId where
    toField (CategoryId cid) = toField cid

instance Query CategoryId where
  parseQuery q = CategoryId <$> (readT =<< "cid" .: q)

newtype Name = Name Text
instance Query Name where
  parseQuery q = Name <$> "name" .: q

data User = User
  { name      :: Text
  , last_name :: Text
  , admin     :: Bool
  , avatar    :: Text
  , reg_date  :: Date
  } deriving (Generic, FromRow, ToJSON)

instance ToJSON Date where
    toJSON (Finite d) = J.String $ pack $ showGregorian d
    toJSON _ = J.Null

data Post = Post
  { _id       :: Int
  , title     :: Text
  , date      :: Date
  , author    :: Author
  , category  :: [Int :. Text]
  , tags      :: [Text]
  , content   :: Text
  , mainImage :: Text
  , images    :: [Text]
  , published :: Bool
  } deriving (Generic)

{-
instance FromRow Post where
  fromRow = Post
    <$> field
    <*> field
    <*> field
    <*> ( Author
            <$> field
            <*> field
            <*> field
        )
    <*> (fromPGArray <$> field)
    <*> (fromPGArray <$> field)
    <*> field
    <*> field
    <*> (fromPGArray <$> field)
    <*> field
-}

readT :: Read a => Text -> Maybe a
readT = fmap fst . listToMaybe . reads . unpack

newtype Title = Title Text
instance Query Title where
  parseQuery q = Title <$> "title" .: q
instance ToField Title where
    toField (Title imgs) = toField imgs

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
  --parseQuery q = CategoryId <$> ( =<< "cid" .: q)

newtype PostId = PostId Int
instance Query PostId where
  parseQuery q = PostId <$> (readT =<< "pid" .: q)

instance ToField Images where
    toField (Images imgs) = toField $ PGArray imgs
