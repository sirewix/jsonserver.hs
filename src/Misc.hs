{-# LANGUAGE FlexibleContexts #-}

module Misc where

import           Control.Arrow                  ( left )
import           Control.Monad.Except           ( MonadError(..), liftEither )
import           Data.Aeson                     ( FromJSON, Value, parseJSON )
import           Data.Aeson.Types               ( parseEither )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Text.Read                      ( readMaybe )

showText :: Show a => a -> Text
showText = pack . show

readT :: Read a => Text -> Maybe a
readT = readMaybe . unpack

readNullable :: Read a => Text -> Maybe (Maybe a)
readNullable text | text == "null" = Just Nothing
readNullable text = Just <$> readT text

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y
infixr 1 ?

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe f mvalue = g =<< mvalue
 where
  g a | f a       = Just a
      | otherwise = Nothing

fromJson :: (MonadError Text m, FromJSON a) => Value -> m a
fromJson = liftEither . left pack . parseEither parseJSON
