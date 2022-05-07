{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Tag where

import Control.Exception (try)
import Control.Monad (void)
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
    Left e -> case sqlErrorMsg e of
      "duplicate key value violates unique constraint \"tag_name_unique\"" -> Left tagNameTaken
      _ -> Left unknownError

delete :: TagId -> Connection -> IO ()
delete tagId conn = do
  _ <- execute conn "DELETE FROM tags WHERE tags.tag_id = ?" (Only tagId)
  return ()

get :: TagId -> Connection -> IO Tag
get tId conn = do
  [(tagId, name)] <-
    query
      conn
      "SELECT tag_id, name FROM tags WHERE tags.tag_id = ?"
      (Only tId)
  return Tag {..}

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

edit :: Tag -> Connection -> IO ()
edit tag conn = do
  _ <-
    execute
      conn
      "UPDATE tags SET name = ? \
      \WHERE tags.tag_id = ?"
      (name tag, tagId tag)
  return ()

doesExist :: TagId -> Connection -> IO (Either Error ())
doesExist tagId conn = do
  [Only n] <-
    query
      conn
      "SELECT COUNT(tag_id) FROM tags \
      \WHERE tags.tag_id = ?"
      (Only tagId)
  if (n :: Integer) == 1
    then return $ Right ()
    else return $ Left tagNotExist
