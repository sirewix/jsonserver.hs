module Misc where
import           Data.Text                      ( Text, pack, unpack)
import           Data.Maybe

showText :: Show a => a -> Text
showText = pack . show

readT :: Read a => Text -> Maybe a
readT = fmap fst . listToMaybe . reads . unpack

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y
infixr 1 ?

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe pfn mvalue = case mvalue of
  Just a | pfn a     -> Nothing
         | otherwise -> mvalue
  Nothing -> Nothing
