module Misc where
import           Data.Text(Text,pack)

showText :: Show a => a -> Text
showText = pack . show

readT :: Read a => Text -> Maybe a
readT = fmap fst . listToMaybe . reads . unpack

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y
infixr 1 ?
