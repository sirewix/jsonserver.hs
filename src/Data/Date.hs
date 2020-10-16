module Data.Date where

import           App.Prototype.Database         ( ToField(..) )
import           Control.Arrow                  ( first )
import           Data.Time.Calendar             ( showGregorian )
import           Data.Text                      ( pack )
import qualified Data.Aeson                    as J
import qualified Database.PostgreSQL.Simple.Time
                                               as T


newtype Date = Date { unDate :: T.Date }

instance ToField Date where
  toField (Date d) = toField d

instance Read Date where
  readsPrec d = map (first Date) . readsPrec d

instance J.ToJSON Date where
  toJSON (Date (T.Finite d)) = J.String . pack $ showGregorian d
  toJSON _                   = J.Null

instance J.FromJSON Date where
  parseJSON v = Date . T.Finite <$> J.parseJSON v
