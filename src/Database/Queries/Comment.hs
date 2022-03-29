{-# LANGUAGE OverloadedStrings #-}
module Database.Queries.Comment where

import Database.PostgreSQL.Simple
import Types.Comment
import Types.Post(PostId)

create :: Comment -> Connection -> IO Bool
create comment conn = do 
    n <- execute conn "INSERT INTO comments (post_id, user_id, text) VALUES (?,?,?)" (postId comment, userId comment, text comment)
    return $ n == 1

get :: PostId -> Connection -> IO [Comment]
get postId conn = do
    xs <- query conn "SELECT comment_id, user_id, text FROM comments WHERE post_id = ?" (Only postId)
    let comments = map (\(commentId, userId, text) -> CommentToGet {
        commentId = commentId,
        userId = userId,
        text = text
    }) xs
    return comments

delete :: CommentId -> Connection -> IO Bool
delete commId conn = do
    n <- execute conn "DELETE FROM comments WHERE comments.comment_id = ?" (Only commId)
    return $ n == 1