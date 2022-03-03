{-# LANGUAGE OverloadedStrings #-}
module Database.Queries.Author where

import Database.PostgreSQL.Simple
import Types.Author
import Data.Text

create :: Author -> Connection -> IO Bool
create author conn = do
    n <- execute conn "INSERT INTO authors (user_id, description) VALUES (?,?)" (userId author, description author)
    return $ n == 1

delete :: AuthorId -> Connection -> IO Bool
delete authorId conn = do 
    n <- execute conn "DELETE FROM authors WHERE authors.author_id = ?" (Only authorId)
    return $ n == 1

get :: AuthorId -> Connection -> IO Author
get authorId conn = do
    [(authorId, userId, descr)] <- query conn 
        "SELECT author_id, user_id, description FROM authors WHERE authors.author_id = ?" (Only authorId)
    return GetAuthor { authorId = authorId,
                    userId = userId,
                    description = descr}

edit :: Author -> Connection -> IO Bool
edit author conn = do 
    n <- execute conn "UPDATE authors SET description = ? WHERE authors.author_id = ?"  (description author, authorId author)
    return $ n == 1

doesExist :: AuthorId -> Connection -> IO Bool
doesExist authorId conn = do
    [Only n] <- query conn "SELECT COUNT(author_id) FROM authors WHERE authors.author_id = ?" (Only authorId)
    return $ (n :: Integer) == 1

