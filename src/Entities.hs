{-# LANGUAGE
  DuplicateRecordFields
#-}
module Entities where
import Data.Text(Text,pack,unpack)

type Date = ()

data Author = Author
    { user_id     :: Int
    , description :: Text
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
