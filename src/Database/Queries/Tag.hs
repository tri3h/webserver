{-# LANGUAGE OverloadedStrings #-}
module Database.Queries.Tag where

import Database.PostgreSQL.Simple
import Types.Tag
import Data.Text

create :: Tag -> Connection -> IO Bool
create tag conn = do
    n <- execute conn "INSERT INTO tags (name) VALUES (?)" (Only $ name tag)
    return $ n == 1

delete :: TagId -> Connection -> IO Bool
delete tagId conn = do 
    n <- execute conn "DELETE FROM tags WHERE tags.tag_id = ?" (Only tagId)
    return $ n == 1

get :: TagId -> Connection -> IO Tag
get tagId conn = do
    [(tagId, name)] <- query conn 
        "SELECT tag_id, name FROM tags WHERE tags.tag_id = ?" (Only tagId)
    return EditTag { tagId = tagId,
                    name = name}

edit :: Tag -> Connection -> IO Bool
edit tag conn = do 
    n <- execute conn "UPDATE tags SET name = ? WHERE tags.tag_id = ?"  (name tag, tagId tag)
    return $ n == 1

doesExist :: TagId -> Connection -> IO Bool
doesExist tagId conn = do
    [Only n] <- query conn "SELECT COUNT(tag_id) FROM tags WHERE tags.tag_id = ?" (Only tagId)
    return $ (n :: Integer) == 1