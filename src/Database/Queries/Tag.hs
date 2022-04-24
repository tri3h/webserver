{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Tag where

import Data.Text (Text)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    execute,
    query,
  )
import Types.PostComment (PostId)
import Types.Tag (Name, Tag (..), TagId, tagNotExist)

create :: Name -> Connection -> IO ()
create name conn = do
  _ <- execute conn "INSERT INTO tags (name) VALUES (?)" (Only name)
  return ()

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

doesExist :: TagId -> Connection -> IO (Either Text ())
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
