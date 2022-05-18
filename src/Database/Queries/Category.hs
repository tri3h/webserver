{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Category where

import Control.Exception (try)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only, fromOnly),
    SqlError (sqlErrorMsg),
    execute,
    query,
    query_,
  )
import Error (Error, categoryNameTaken, categoryNotExist, invalidParent, unknownError)
import Types.Category
  ( CategoryId,
    CreateCategory (..),
    GetCategory (..),
    Name,
    ParentId,
  )

create :: CreateCategory -> Connection -> IO (Either Error ())
create cat conn = do
  result <-
    try $
      execute
        conn
        "INSERT INTO categories (name, parent_id) \
        \VALUES (?,?)"
        (cName cat, cParentId cat)
  return $ case result of
    Right _ -> Right ()
    Left l -> case sqlErrorMsg l of
      "duplicate key value violates unique constraint \"category_name_unique\"" -> Left categoryNameTaken
      "insert or update on table \"categories\" violates foreign key constraint \"parent_id\"" -> Left invalidParent
      _ -> Left unknownError

delete :: CategoryId -> Connection -> IO (Either Error ())
delete catId conn = do
  result <-
    execute
      conn
      "DELETE FROM categories \
      \WHERE categories.category_id = ?"
      (Only catId)
  return $ if result == 0 then Left categoryNotExist else Right ()

get :: CategoryId -> Connection -> IO (Either Error GetCategory)
get catId conn = do
  result <-
    query
      conn
      "SELECT category_id, name, parent_id FROM categories \
      \WHERE category_id = ?"
      (Only catId)
  return $
    if null result
      then Left categoryNotExist
      else Right $ (\[(gCategoryId, gName, gParentId)] -> GetCategory {..}) result

getAll :: Connection -> IO [GetCategory]
getAll conn = do
  result <- query_ conn "SELECT category_id, name, parent_id FROM categories"
  return $ map (\(gCategoryId, gName, gParentId) -> GetCategory {..}) result

editName :: CategoryId -> Name -> Connection -> IO (Either Error ())
editName categoryId name conn = do
  result <-
    try $
      execute
        conn
        "UPDATE categories SET name = ? \
        \WHERE category_id = ?"
        (name, categoryId)
  return $ case result of
    Right n -> if n == 0 then Left categoryNotExist else Right ()
    Left l -> case sqlErrorMsg l of
      "duplicate key value violates unique constraint \"category_name_unique\"" -> Left categoryNameTaken
      _ -> Left unknownError

editParent :: CategoryId -> ParentId -> Connection -> IO (Either Error ())
editParent categoryId parentId conn = do
  result <-
    try $
      execute
        conn
        "UPDATE categories SET parent_id = ? \
        \WHERE category_id = ?"
        (parentId, categoryId)
  return $ case result of
    Right n -> if n == 0 then Left categoryNotExist else Right ()
    Left l -> case sqlErrorMsg l of
      "insert or update on table \"categories\" violates foreign key constraint \"parent_id\"" -> Left invalidParent
      _ -> Left unknownError

getWithParents :: Maybe CategoryId -> Connection -> IO [GetCategory]
getWithParents catId conn = do
  xs <-
    query
      conn
      "WITH RECURSIVE parents AS (SELECT category_id, name, \
      \parent_id FROM categories WHERE category_id = ? \
      \UNION SELECT c.category_id, c.name, c.parent_id FROM categories c, \
      \parents p WHERE c.category_id = p.parent_id) SELECT category_id, name, parent_id \
      \FROM parents"
      (Only catId)
  return $ map (\(gCategoryId, gName, gParentId) -> GetCategory {..}) xs

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
