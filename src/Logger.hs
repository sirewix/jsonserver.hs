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

import           Control.Concurrent             ( MVar
                                                , withMVar
                                                )
import           Control.Monad                  ( when )
import           Data.Aeson                     ( FromJSON )
import           Data.Text                      ( Text )
import           Data.Text.IO                   ( hPutStrLn )
import           Data.Time.Clock                ( getCurrentTime )
import           GHC.Generics                   ( Generic )
import           Misc                           ( showText )
import qualified System.IO

data Priority
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Priority

type Logger = Priority -> Text -> IO ()

sublog :: Text -> Logger -> Logger
sublog prefix logger prio msg = logger prio (prefix <> msg)

newLogger :: MVar System.IO.Handle -> Priority -> Logger
newLogger h logPrio prio msg = when (prio >= logPrio) $ do
    now <- getCurrentTime
    withMVar h $ \h -> hPutStrLn h $ showText now <> " [" <> showText prio <> "] " <> msg
