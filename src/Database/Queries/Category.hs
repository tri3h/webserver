{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Category where

import Data.Text (Text)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only, fromOnly),
    execute,
    query,
    query_,
  )
import Types.Category
  ( CategoryId,
    CreateCategory (..),
    GetCategory (..),
    Name,
    ParentId,
    categoryNotExist,
  )

create :: CreateCategory -> Connection -> IO ()
create cat conn = do
  _ <-
    execute
      conn
      "INSERT INTO categories (name, parent_id) \
      \VALUES (?,?)"
      (cName cat, cParentId cat)
  return ()

delete :: CategoryId -> Connection -> IO ()
delete catId conn = do
  _ <-
    execute
      conn
      "DELETE FROM categories \
      \WHERE categories.category_id = ?"
      (Only catId)
  return ()

get :: CategoryId -> Connection -> IO GetCategory
get catId conn = do
  [(gCategoryId, gName, gParentId)] <-
    query
      conn
      "SELECT category_id, name, parent_id FROM categories \
      \WHERE categories.category_id = ?"
      (Only catId)
  return GetCategory {..}

getAll :: Connection -> IO [GetCategory]
getAll conn = do
  result <- query_ conn "SELECT category_id, name, parent_id FROM categories"
  return $ map (\(gCategoryId, gName, gParentId) -> GetCategory {..}) result

editName :: CategoryId -> Name -> Connection -> IO ()
editName categoryId name conn = do
  _ <-
    execute
      conn
      "UPDATE categories SET name = ? \
      \WHERE categories.category_id = ?"
      (name, categoryId)
  return ()

editParent :: CategoryId -> ParentId -> Connection -> IO ()
editParent categoryId parentId conn = do
  _ <-
    execute
      conn
      "UPDATE categories SET parent_id = ? \
      \WHERE categories.category_id = ?"
      (parentId, categoryId)
  return ()

doesExist :: CategoryId -> Connection -> IO (Either Text ())
doesExist catId conn = do
  [Only n] <-
    query
      conn
      "SELECT COUNT(category_id) FROM categories \
      \WHERE categories.category_id = ?"
      (Only catId)
  if (n :: Integer) == 1
    then return $ Right ()
    else return $ Left categoryNotExist

getParents :: CategoryId -> Connection -> IO [CategoryId]
getParents catId conn = do
  xs <-
    query
      conn
      "WITH RECURSIVE parents AS (SELECT category_id, \
      \parent_id FROM categories WHERE category_id = ? \
      \UNION SELECT c.category_id, c.parent_id FROM categories c, \
      \parents p WHERE c.category_id = p.parent_id) SELECT category_id \
      \FROM parents"
      (Only catId)
  return $ map fromOnly xs

getChildren :: CategoryId -> Connection -> IO [CategoryId]
getChildren parId conn = do
  xs <-
    query
      conn
      "WITH RECURSIVE children AS (SELECT category_id, \
      \parent_id FROM categories WHERE parent_id = ? \
      \UNION SELECT c.category_id, c.parent_id FROM categories c, \
      \children ch WHERE ch.category_id = c.parent_id) SELECT category_id \
      \FROM children"
      (Only parId)
  return $ map fromOnly xs
