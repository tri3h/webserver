{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Comment where

import Data.Text (Text)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    execute,
    query,
  )
import Types.Comment
  ( CommentId,
    CreateComment (..),
    GetComment (..),
    commentNotExist,
  )
import Types.PostComment (PostId)

create :: CreateComment -> Connection -> IO ()
create comment conn = do
  _ <-
    execute
      conn
      "INSERT INTO comments (post_id, user_id, text) \
      \VALUES (?,?,?)"
      (cPostId comment, cUserId comment, cText comment)
  return ()

get :: PostId -> Connection -> IO [GetComment]
get postId conn = do
  xs <-
    query
      conn
      "SELECT comment_id, user_id, text FROM comments \
      \WHERE post_id = ?"
      (Only postId)
  let comments =
        map
          ( \(gCommentId, gUserId, gText) ->
              GetComment {..}
          )
          xs
  return comments

delete :: CommentId -> Connection -> IO ()
delete commId conn = do
  _ <- execute conn "DELETE FROM comments WHERE comments.comment_id = ?" (Only commId)
  return ()

doesExist :: CommentId -> Connection -> IO (Either Text ())
doesExist commId conn = do
  [Only n] <-
    query
      conn
      "SELECT COUNT(comment_id) FROM comments \
      \WHERE comments.comment_id = ?"
      (Only commId)
  if (n :: Integer) == 1
    then return $ Right ()
    else return $ Left commentNotExist
