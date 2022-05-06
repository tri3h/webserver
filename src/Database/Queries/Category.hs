{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Category where

import Control.Exception (try)
import Control.Monad (void)
import Data.Text (Text)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only, fromOnly),
    SqlError (sqlErrorMsg),
    execute,
    query,
    query_,
  )
import Error (unknownError)
import Types.Category
  ( CategoryId,
    CreateCategory (..),
    GetCategory (..),
    Name,
    ParentId,
    categoryNameTaken,
    categoryNotExist,
  )

create :: CreateCategory -> Connection -> IO (Either Text ())
create cat conn = do
  result <-
    try . void $
      execute
        conn
        "INSERT INTO categories (name, parent_id) \
        \VALUES (?,?)"
        (cName cat, cParentId cat)
  return $ case result of
    Right _ -> Right ()
    Left e -> case sqlErrorMsg e of
      "duplicate key value violates unique constraint \"category_name_unique\"" -> Left categoryNameTaken
      _ -> Left unknownError

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
