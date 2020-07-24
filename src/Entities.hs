{-# LANGUAGE
  DuplicateRecordFields
, OverloadedStrings
, DeriveAnyClass
, DeriveGeneric
, TypeSynonymInstances
, FlexibleInstances
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
  { username    :: Text
  , description :: Text
  } deriving (Generic, FromRow, ToJSON)

instance Query Author where
  parseQuery q = Author
    <$> ("username" .: q)
    <*> ("description" .: q)

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
  parseQuery q = CategoryId <$> (fmap fst . listToMaybe . reads . unpack =<< "cid" .: q)

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

    {-
data Category =
    Category Text
  | SubCategory Category Text

type Image = ()

data Post = Post
    { _id       :: Int
    , title     :: Text
    , date      :: Date
    , author    :: Author
    , category  :: Category
    , tags      :: [Tag]
    , content   :: Text
    , mainImage :: Image
    , images    :: [Image]
    }

newtype PostDraft = PostDraft (Post)

{-
Черновики
    Новость должна иметь возможность иметь черновики — то есть мы должны иметь возможность вносить изменения, но не опубликовать их.
    По АПИ отдаем только опубликованные новости.
    Только автор может видеть черновик к новости и изменять его.
-}
-}
