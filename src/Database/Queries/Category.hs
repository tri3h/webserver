{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.Category where

import Database.PostgreSQL.Simple
    ( Connection, execute, query, Only(Only, fromOnly) )
import Types.Category
    ( Category(Category, name, parentId, categoryId), CategoryId )
import Data.Text ( Text )

create :: Category -> Connection -> IO ()
create cat conn = do
    execute conn "INSERT INTO categories (name, parent_id) \
        \VALUES (?,?)" (name cat, parentId cat)
    return ()

delete :: CategoryId -> Connection -> IO ()
delete catId conn = do 
    execute conn "DELETE FROM categories \
        \WHERE categories.category_id = ?" (Only catId)
    return ()

get :: CategoryId -> Connection -> IO Category
get catId conn = do
    [(categoryId, name, parentId)] <- query conn 
        "SELECT category_id, name, parent_id FROM categories \
        \WHERE categories.category_id = ?" (Only catId)
    return Category { .. }

edit :: Category -> Connection -> IO ()
edit cat conn = do 
    case parentId cat of 
        Nothing -> execute conn "UPDATE categories SET name = ? \
            \WHERE categories.category_id = ?"  (name cat, categoryId cat)
        _ -> execute conn "UPDATE categories SET name = ?, \
            \parent_id = ? WHERE categories.category_id = ?"  
            (name cat, parentId cat, categoryId cat)
    return ()

doesExist :: CategoryId -> Connection -> IO (Either Text ())
doesExist catId conn = do 
    [Only n] <- query conn "SELECT COUNT(category_id) FROM categories \
        \WHERE categories.category_id = ?" (Only catId)
    if (n :: Integer) == 1
    then return $ Right ()
    else return $ Left "Category with such id doesn't exist"

getParents :: CategoryId -> Connection -> IO [CategoryId]
getParents catId conn = do 
    xs <- query conn "WITH RECURSIVE parents AS (SELECT category_id, \
        \parent_id FROM categories WHERE category_id = ? \
        \UNION SELECT c.category_id, c.parent_id FROM categories c, \
        \parents p WHERE c.category_id = p.parent_id) SELECT category_id \
        \FROM parents" (Only catId)
    return $ map fromOnly xs

getChildren :: CategoryId -> Connection -> IO [CategoryId]
getChildren parId conn = do 
    xs <- query conn "WITH RECURSIVE children AS (SELECT category_id, \
        \parent_id FROM categories WHERE parent_id = ? \
        \UNION SELECT c.category_id, c.parent_id FROM categories c, \
        \children ch WHERE ch.category_id = c.parent_id) SELECT category_id \
        \FROM children" (Only parId)        
    let xs' = map (\(Only x) -> x) xs
    return xs'

