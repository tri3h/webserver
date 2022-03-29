{-# LANGUAGE OverloadedStrings #-}
module Database.Queries.Tag where

import Database.PostgreSQL.Simple
import Types.Tag
import Types.Post(PostId)
import Data.Text(Text)

create :: Name -> Connection -> IO Bool
create name conn = do
    n <- execute conn "INSERT INTO tags (name) VALUES (?)" (Only name)
    return $ n == 1

delete :: TagId -> Connection -> IO Bool
delete tagId conn = do 
    n <- execute conn "DELETE FROM tags WHERE tags.tag_id = ?" (Only tagId)
    return $ n == 1

get :: TagId -> Connection -> IO Tag
get tagId conn = do
    [(tagId, name)] <- query conn 
        "SELECT tag_id, name FROM tags WHERE tags.tag_id = ?" (Only tagId)
    return Tag { tagId = tagId,
                    name = name}

getByPostId :: PostId -> Connection -> IO [Tag]
getByPostId postId conn = do
    xs <- query conn 
        "SELECT t.tag_id, t.name FROM tags t INNER JOIN post_tags pt \
        \ON t.tag_id = pt.tag_id WHERE pt.post_id = ?" (Only postId)
    let xs' = map (\(tagId, name) -> Tag { tagId = tagId,
                                                name = name}) xs
    return xs'

edit :: Tag -> Connection -> IO Bool
edit tag conn = do 
    n <- execute conn "UPDATE tags SET name = ? WHERE tags.tag_id = ?"  (name tag, tagId tag)
    return $ n == 1

doesExist :: TagId -> Connection -> IO Bool
doesExist tagId conn = do
    [Only n] <- query conn "SELECT COUNT(tag_id) FROM tags WHERE tags.tag_id = ?" (Only tagId)
    return $ (n :: Integer) == 1