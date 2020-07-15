{-# LANGUAGE
  DuplicateRecordFields
, OverloadedStrings
#-}
module Entities where
import Data.Text(Text,pack,unpack)
import Query
import           Data.Text.Encoding

newtype UserName = UserName Text
newtype LastName = LastName Text
newtype Password = Password Text

instance Query UserName where
  parseQuery q = UserName . decodeUtf8 <$> "name" .: q

instance Query LastName where
  parseQuery q = LastName . decodeUtf8 <$> "lastname" .: q

instance Query Password where
  parseQuery q = Password . decodeUtf8 <$> "password" .: q

newtype Description = Description Text
instance Query Description where
  parseQuery q = Description . decodeUtf8 <$> "description" .: q

newtype Token = Token Text
instance Query Token where
  parseQuery q = Token . decodeUtf8 <$> "token" .: q

newtype BackdooredUser = BackdooredUser { unBackdooredUser :: Text }
instance Query BackdooredUser where
  parseQuery q = BackdooredUser . decodeUtf8 <$> "backdoor" .: q

data Author = Author
    { username    :: Text
    , description :: Text
    }

instance Query Author where
    parseQuery q = Author
        <$> (decodeUtf8 <$> "username" .: q)
        <*> (decodeUtf8 <$> "description" .: q)

    {-
type Date = ()

data User = User
  { name             :: Text
  , lastName         :: Text
  , avatar           :: Text
  -- , registrationDate :: Integer
  , admin            :: Bool
  , author           :: Bool
  }

data Category =
    Category Text
  | SubCategory Category Text

type Image = ()
type Tag = Text

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