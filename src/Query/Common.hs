{-# LANGUAGE
    DeriveAnyClass
  , DeriveGeneric
  , TypeSynonymInstances
  #-}

module Query.Common where

import           App.Prototype.Database         ( Id(..) )
import           Data.Text                      ( Text )
import           Misc                           ( readT )
import           Query.FromQuery                ( FromQuery(..)
                                                , filterQuery
                                                , liftMaybe
                                                , param
                                                , opt
                                                )

newtype QueryId a = QueryId { getQueryId :: Id a }

instance FromQuery (QueryId a) where
  parseQuery = fmap QueryId $ fmap Id
    . filterQuery (> 0)
    $ liftMaybe . readT =<< param "id"

newtype Page = Page Int
instance FromQuery Page where
  parseQuery = fmap Page
    . filterQuery (> 0)
    $ liftMaybe . maybe (Just 1) readT =<< opt "page"

newtype Token = Token Text
instance FromQuery Token where
  parseQuery = Token <$> param "token"
