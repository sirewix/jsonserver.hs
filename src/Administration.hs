{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
#-}
module Administration
    ( make_author
    ) where

import           App
import           Entities
import           Database.PostgreSQL.Simple
import           Data.String (fromString)
import qualified Data.Aeson                    as J
import           Control.Exception

make_author (UserName admin) (UserName name, Description description) db = do
    {-
  dbres <- flip (execute db) () . fromString $
       "WITH moved_rows AS ("
    <>     "DELETE FROM <original_table> a"
    <>     "USING <other_table> b"
    <>     "WHERE <condition>"
    <>     "RETURNING a.*" -- or specify columns
    <> ") INSERT INTO <existing_table>" --specify columns if necessary
    <> "SELECT [DISTINCT] * FROM moved_rows;"
    -}
  print (admin, name, description)

  flip catches (Handler (\(e :: QueryError) -> return BadRequest) : defaultDbHandlers) $ do
      execute db "INSERT INTO authors (username, description) VALUES ?" (name, description)
      return . AppOk $ J.Null

edit_author (UserName name, Description description) db = undefined
delete_author (UserName name) db = undefined
