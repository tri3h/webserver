{-# LANGUAGE OverloadedStrings #-}

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
  )
import Error (Error, tagNameTaken, tagNotExist, unknownError)
import Types.Limit (Limit, Offset)
import Types.PostComment (PostId)
import qualified Types.Tag as T

create :: T.Name -> Connection -> IO (Either Error ())
create name conn = do
  result <- try . void $ execute conn "INSERT INTO tags (name) VALUES (?)" (Only name) :: IO (Either SqlError ())
  return $ case result of
    Right _ -> Right ()
    Left l -> case sqlErrorMsg l of
      "duplicate key value violates unique constraint \"tag_name_unique\"" -> Left tagNameTaken
      _ -> Left unknownError

delete :: T.TagId -> Connection -> IO (Either Error ())
delete tagId conn = do
  result <- execute conn "DELETE FROM tags WHERE tags.tag_id = ?" (Only tagId)
  return $ if result == 0 then Left tagNotExist else Right ()

get :: T.TagId -> Connection -> IO (Either Error T.Tag)
get tId conn = do
  result <-
    query
      conn
      "SELECT tag_id, name FROM tags WHERE tags.tag_id = ?"
      (Only tId)
  return $
    if null result
      then Left tagNotExist
      else Right $ (\[(tagId, name)] -> T.Tag {T.tagId = tagId, T.name = name}) result

getAll :: Limit -> Offset -> Connection -> IO [T.Tag]
getAll limit offset conn = do
  result <- query conn "SELECT tag_id, name FROM tags LIMIT ? OFFSET ?" (limit, offset)
  return $ map (\(tagId, name) -> T.Tag {T.tagId = tagId, T.name = name}) result

getByPostId :: PostId -> Connection -> IO [T.Tag]
getByPostId postId conn = do
  xs <-
    query
      conn
      "SELECT t.tag_id, t.name FROM tags t \
      \INNER JOIN post_tags pt ON t.tag_id = pt.tag_id \
      \WHERE pt.post_id = ?"
      (Only postId)
  let xs' = map (\(tagId, name) -> T.Tag {T.tagId = tagId, T.name = name}) xs
  return xs'

edit :: T.Tag -> Connection -> IO (Either Error ())
edit tag conn = do
  result <-
    try $
      execute
        conn
        "UPDATE tags SET name = ? \
        \WHERE tags.tag_id = ?"
        (T.name tag, T.tagId tag) ::
      IO (Either SqlError Int64)
  return $ case result of
    Right r -> if r == 0 then Left tagNotExist else Right ()
    Left l -> case sqlErrorMsg l of
      "duplicate key value violates unique constraint \"tag_name_unique\"" -> Left tagNameTaken
      _ -> Left unknownError
