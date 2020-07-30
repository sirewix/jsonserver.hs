module Misc where
import           Data.Text(Text,pack)

showText :: Show a => a -> Text
showText = pack . show

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y
infixr 1 ?
