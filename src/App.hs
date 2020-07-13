{-# LANGUAGE ScopedTypeVariables #-}
module App
    ( AppResponse(..)
    , defaultDbHandlers
    ) where

import           Control.Exception
import           Database.PostgreSQL.Simple hiding ( Query )
import qualified Data.Aeson                    as J

data AppResponse =
    AppOk J.Value
  | BadRequest
  | InternalError
  | AccessDenied
  | TokenExpired

defaultDbHandlers =
  [ Handler (\(e :: FormatError) -> return InternalError)
  , Handler (\(e :: ResultError) -> return InternalError)
  , Handler (\(e :: SqlError) -> return InternalError)
  ]

