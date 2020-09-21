module Misc where
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Text.Read                      ( readMaybe )

showText :: Show a => a -> Text
showText = pack . show

readT :: Read a => Text -> Maybe a
readT = readMaybe . unpack

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y
infixr 1 ?

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe f mvalue = g =<< mvalue
 where
  g a | f a       = Just a
      | otherwise = Nothing
