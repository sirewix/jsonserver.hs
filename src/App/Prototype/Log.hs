{-# LANGUAGE DeriveGeneric #-}
module App.Prototype.Log where

import           Data.Aeson                     ( FromJSON )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data Priority
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Priority

class HasLog m where
  log' :: Priority -> Text -> m ()
