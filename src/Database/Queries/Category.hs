{-# LANGUAGE OverloadedStrings #-}
module Database.Queries.Category where

import Database.PostgreSQL.Simple
import Types.Category
import Control.Monad

create :: Category -> Connection -> IO Bool
create cat conn = do
    n <- execute conn "INSERT INTO categories (name, parent_id) VALUES (?,?)" (name cat, parentId cat)
    return $ n == 1

delete :: CategoryId -> Connection -> IO Bool
delete catId conn = do 
    n <- execute conn "DELETE FROM categories WHERE categories.category_id = ?" (Only catId)
    return $ n == 1

get :: CategoryId -> Connection -> IO Category
get catId conn = do
    [(catId, name, parentId)] <- query conn 
        "SELECT category_id, name, parent_id FROM categories WHERE categories.category_id = ?" (Only catId)
    return GetCategory { categoryId = catId,
                    name = name,
                    parentId = parentId}

edit :: Category -> Connection -> IO Bool
edit cat conn = do 
    n <- case parentId cat of 
            Nothing -> execute conn "UPDATE categories SET name = ? WHERE categories.category_id = ?"  (name cat, categoryId cat)
            _ -> execute conn "UPDATE categories SET name = ?, parent_id = ? WHERE categories.category_id = ?"  (name cat, parentId cat, categoryId cat)
    return $ n == 1

doesExist :: CategoryId -> Connection -> IO Bool
doesExist catId conn = do 
    [Only n] <- query conn "SELECT COUNT(category_id) FROM categories WHERE categories.category_id = ?" (Only catId)
    return $ (n :: Integer) == 1

getParents :: CategoryId -> Connection -> IO [CategoryId]
getParents catId conn = do 
    xs <- query conn "WITH RECURSIVE parents AS (SELECT category_id, parent_id FROM categories WHERE category_id = ? UNION SELECT c.category_id, c.parent_id FROM categories c, parents p WHERE c.category_id = p.parent_id) SELECT category_id FROM parents" (Only catId)
    return $ map fromOnly xs

getChildren :: CategoryId -> Connection -> IO [CategoryId]
getChildren parId conn = do 
    xs <- query conn "WITH RECURSIVE children AS (SELECT category_id, parent_id FROM categories WHERE parent_id = ? UNION SELECT c.category_id, c.parent_id FROM categories c, children ch WHERE ch.category_id = c.parent_id) SELECT category_id FROM children" (Only parId)        
    let xs' = map (\(Only x) -> x) xs
    return xs'

