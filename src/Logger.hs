{-# LANGUAGE
  DeriveGeneric
, OverloadedStrings
#-}
module Logger
  ( Priority(..)
  , Logger
  , sublog
  , newLogger
  )
where

import           Control.Concurrent
import           Data.Aeson                     ( FromJSON )
import           Data.Text                      ( Text )
import           Data.Text.IO                   ( hPutStrLn )
import           Data.Time.Clock
import           GHC.Generics
import           Misc
import qualified System.IO                     as System.IO

data Priority
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Priority

type Logger = Priority -> Text -> IO ()

sublog :: Text -> Logger -> Logger
sublog prefix logger = \prio msg -> logger prio (prefix <> msg)

newLogger :: MVar System.IO.Handle -> Priority -> Logger
newLogger h logPrio = \prio msg -> if prio >= logPrio
  then do
    now <- getCurrentTime
    withMVar h $ \h -> hPutStrLn h $ showText now <> " [" <> showText prio <> "] " <> msg
  else return ()
