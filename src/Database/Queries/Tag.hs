{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Tag where

import Control.Exception (try)
import Control.Monad (void)
import Data.Int (Int64)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    SqlError (sqlErrorMsg),
    execute,
    query,
    query_,
  )
import Error (Error, tagNameTaken, tagNotExist, unknownError)
import Types.PostComment (PostId)
import Types.Tag (Name, Tag (..), TagId)

create :: Name -> Connection -> IO (Either Error ())
create name conn = do
  result <- try . void $ execute conn "INSERT INTO tags (name) VALUES (?)" (Only name) :: IO (Either SqlError ())
  return $ case result of
    Right _ -> Right ()
    Left l -> case sqlErrorMsg l of
      "duplicate key value violates unique constraint \"tag_name_unique\"" -> Left tagNameTaken
      _ -> Left unknownError

delete :: TagId -> Connection -> IO (Either Error ())
delete tagId conn = do
  result <- execute conn "DELETE FROM tags WHERE tags.tag_id = ?" (Only tagId)
  return $ if result == 0 then Left tagNotExist else Right ()

get :: TagId -> Connection -> IO (Either Error Tag)
get tId conn = do
  result <-
    query
      conn
      "SELECT tag_id, name FROM tags WHERE tags.tag_id = ?"
      (Only tId)
  return $
    if null result
      then Left tagNotExist
      else Right $ (\[(tagId, name)] -> Tag {..}) result

getAll :: Connection -> IO [Tag]
getAll conn = do
  result <- query_ conn "SELECT tag_id, name FROM tags"
  return $ map (\(tagId, name) -> Tag {..}) result

getByPostId :: PostId -> Connection -> IO [Tag]
getByPostId postId conn = do
  xs <-
    query
      conn
      "SELECT t.tag_id, t.name FROM tags t \
      \INNER JOIN post_tags pt ON t.tag_id = pt.tag_id \
      \WHERE pt.post_id = ?"
      (Only postId)
  let xs' = map (\(tagId, name) -> Tag {..}) xs
  return xs'

edit :: Tag -> Connection -> IO (Either Error ())
edit tag conn = do
  result <-
    try $
      execute
        conn
        "UPDATE tags SET name = ? \
        \WHERE tags.tag_id = ?"
        (name tag, tagId tag) ::
      IO (Either SqlError Int64)
  return $ case result of
    Right r -> if r == 0 then Left tagNotExist else Right ()
    Left l -> case sqlErrorMsg l of
      "duplicate key value violates unique constraint \"tag_name_unique\"" -> Left tagNameTaken
      _ -> Left unknownError
