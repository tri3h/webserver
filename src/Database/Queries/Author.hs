{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.Author where

import Database.PostgreSQL.Simple
    ( Connection, execute, query, Only(Only) )
import Types.Author
    ( Author(AuthorToGet, userId, description, authorId), AuthorId, authorNotExist )
import Types.Post (PostId)
import Types.Draft (DraftId)
import Types.User(Token)
import Data.Text ( Text )
import Data.Maybe ( fromMaybe )

create :: Author -> Connection -> IO ()
create author conn = do
    execute conn "INSERT INTO authors (user_id, description) \
        \VALUES (?,?)" (userId author, description author)
    return ()

delete :: AuthorId -> Connection -> IO ()
delete authorId conn = do
    execute conn "DELETE FROM authors WHERE authors.author_id = ?" (Only authorId)
    return ()

get :: AuthorId -> Connection -> IO Author
get authorId conn = do
    [(authorId, maybeUserId, description)] <- query conn
        "SELECT author_id, user_id, description FROM authors \
        \WHERE authors.author_id = ?" (Only authorId)
    let userId = fromMaybe 0 maybeUserId
    return AuthorToGet { .. }

getMaybe :: AuthorId -> Connection -> IO (Maybe Author)
getMaybe authorId conn = do
    x <- query conn
        "SELECT author_id, user_id, description FROM authors \
        \WHERE authors.author_id = ?" (Only authorId)
    case x of 
        [(authorId, maybeUserId, description)] -> do 
            let userId = fromMaybe 0 maybeUserId
            return $ Just AuthorToGet { .. }
        _ -> return Nothing

edit :: Author -> Connection -> IO ()
edit author conn = do
    execute conn "UPDATE authors SET description = ? \
        \WHERE authors.author_id = ?"  (description author, authorId author)
    return ()

doesExist :: AuthorId -> Connection -> IO (Either Text ())
doesExist authorId conn = do
    [Only n] <- query conn "SELECT COUNT(author_id) FROM authors \
        \WHERE authors.author_id = ?" (Only authorId)
    if (n :: Integer) == 1
    then return $ Right ()
    else return $ Left authorNotExist

getByPostId :: PostId -> Connection -> IO AuthorId
getByPostId postId conn = do
    [Only authorId] <- query conn "SELECT author_id FROM posts \
        \WHERE post_id = ?" (Only postId)
    return authorId

getByDraftId :: DraftId -> Connection -> IO AuthorId
getByDraftId draftId conn = do
    [Only authorId] <- query conn "SELECT author_id FROM drafts \
        \WHERE draft_id = ?" (Only draftId)
    return authorId

getByToken :: Token -> Connection -> IO AuthorId
getByToken token conn = do
    [Only authorId] <- query conn "SELECT author_id FROM authors a INNER JOIN \
        \users u ON a.user_id = u.user_id WHERE u.token = ?" (Only token)
    return authorId

isAuthor :: Token -> Connection -> IO Bool
isAuthor token conn = do
    [Only n] <- query conn "SELECT COUNT(author_id) FROM authors a \
        \INNER JOIN users u ON a.user_id = u.user_id WHERE token = ?" (Only token)
    return $ (n :: Integer) == 1
