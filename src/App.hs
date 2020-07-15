{-# LANGUAGE ScopedTypeVariables #-}
module App
    ( AppResponse(..)
    , defaultDbHandlers
    , catchDb
    ) where

import           Misc
import           Control.Exception
import           Database.PostgreSQL.Simple hiding ( Query )
import           Logger
import qualified Data.Aeson                    as J

data AppResponse =
    AppOk J.Value
  | BadRequest
  | InternalError
  | AccessDenied
  | TokenExpired

defaultDbHandlers log =
  [ Handler (\(e :: FormatError) -> log Error (showText e) >> return InternalError)
  , Handler (\(e :: ResultError) -> log Error (showText e) >> return InternalError)
  , Handler (\(e :: SqlError   ) -> log Error (showText e) >> return InternalError)
  ]

catchDb log ret = flip catches (Handler (\(e :: QueryError) -> ret) : defaultDbHandlers log)
