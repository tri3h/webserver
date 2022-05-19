{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Comment where

import Control.Exception (try)
import Control.Monad (void)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    SqlError (sqlErrorMsg),
    execute,
    query,
  )
import Error (Error, commentNotExist, postNotExist, unknownError)
import Types.Comment
  ( CommentId,
    CreateComment (..),
    GetComment (..),
  )
import Types.PostComment (PostId)

create :: CreateComment -> Connection -> IO (Either Error ())
create comment conn = do
  result <-
    try . void $
      execute
        conn
        "INSERT INTO comments (post_id, user_id, text) \
        \VALUES (?,(SELECT user_id FROM users WHERE token = ?),?)"
        (cPostId comment, cToken comment, cText comment) ::
      IO (Either SqlError ())
  return $ case result of
    Right _ -> Right ()
    Left l -> case sqlErrorMsg l of
      "insert or update on table \"comments\" violates foreign key constraint \"post_id\"" -> Left postNotExist
      _ -> Left unknownError

getEither :: PostId -> Connection -> IO (Either Error [GetComment])
getEither postId conn = do
  [Only countPost] <- query conn "SELECT COUNT(post_id) FROM posts WHERE post_id = ?" (Only postId)
  if (countPost :: Integer) /= 0
    then do
      xs <-
        query
          conn
          "SELECT comment_id, user_id, text FROM comments WHERE post_id = ?"
          (Only postId)
      return . Right $
        map
          ( \(gCommentId, gUserId, gText) ->
              GetComment {..}
          )
          xs
    else return $ Left postNotExist

get :: PostId -> Connection -> IO [GetComment]
get postId conn = do
  xs <-
    query
      conn
      "SELECT comment_id, user_id, text FROM comments WHERE post_id = ?"
      (Only postId)
  return $
    map
      ( \(gCommentId, gUserId, gText) ->
          GetComment {..}
      )
      xs

delete :: CommentId -> Connection -> IO (Either Error ())
delete commId conn = do
  result <- execute conn "DELETE FROM comments WHERE comments.comment_id = ?" (Only commId)
  return $ if result == 0 then Left commentNotExist else Right ()
