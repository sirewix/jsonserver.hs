{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
#-}
module Administration
    ( make_author
    ) where

import           App
import           Misc
import           Entities
import           Database.PostgreSQL.Simple
import           Data.String (fromString)
import qualified Data.Aeson                    as J
import           Control.Exception
import           Logger

make_author (UserName admin) (UserName name, Description description) (log, db) = do
  log Info $ admin <> " promoted '" <> name <> "' to authors with description \"" <> description <> "\""

  flip catches (Handler (\(e :: QueryError) -> return BadRequest) : defaultDbHandlers log) $ do
      execute db "INSERT INTO authors (username, description) VALUES (?, ?)" (name, description)
      return . AppOk $ J.Null

edit_author (UserName name, Description description) db = undefined
delete_author (UserName name) db = undefined
