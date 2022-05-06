{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Author where

import Control.Exception (try)
import Control.Monad (void)
import Data.Text (Text)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    SqlError (sqlErrorMsg),
    execute,
    query,
  )
import Error (unknownError)
import Types.Author
  ( AuthorId,
    CreateAuthor (..),
    EditAuthor (..),
    GetAuthor (..),
    alreadyAuthor,
    authorNotExist,
  )
import Types.Draft (DraftId)
import Types.PostComment (PostId)
import Types.User (Token, userNotExist)

create :: CreateAuthor -> Connection -> IO (Either Text ())
create author conn = do
  result <-
    try . void $
      execute
        conn
        "INSERT INTO authors (user_id, description) \
        \VALUES (?,?)"
        (cUserId author, cDescription author) ::
      IO (Either SqlError ())
  return $ case result of
    Right _ -> Right ()
    Left e -> case sqlErrorMsg e of
      "duplicate key value violates unique constraint \"user_id_unique\"" -> Left alreadyAuthor
      "insert or update on table \"authors\" violates foreign key constraint \"user_id\"" -> Left userNotExist
      _ -> Left unknownError

delete :: AuthorId -> Connection -> IO ()
delete authorId conn = do
  _ <- execute conn "DELETE FROM authors WHERE authors.author_id = ?" (Only authorId)
  return ()

get :: AuthorId -> Connection -> IO GetAuthor
get authId conn = do
  [(gAuthorId, gUserId, gDescription)] <-
    query
      conn
      "SELECT author_id, user_id, description FROM authors \
      \WHERE authors.author_id = ?"
      (Only authId)
  return GetAuthor {..}

getMaybe :: AuthorId -> Connection -> IO (Maybe GetAuthor)
getMaybe authId conn = do
  x <-
    query
      conn
      "SELECT author_id, user_id, description FROM authors \
      \WHERE authors.author_id = ?"
      (Only authId)
  case x of
    [(gAuthorId, gUserId, gDescription)] ->
      return $ Just GetAuthor {..}
    _ -> return Nothing

edit :: EditAuthor -> Connection -> IO ()
edit author conn = do
  _ <-
    execute
      conn
      "UPDATE authors SET description = ? \
      \WHERE authors.author_id = ?"
      (eDescription author, eAuthorId author)
  return ()

doesExist :: AuthorId -> Connection -> IO (Either Text ())
doesExist authorId conn = do
  [Only n] <-
    query
      conn
      "SELECT COUNT(author_id) FROM authors \
      \WHERE authors.author_id = ?"
      (Only authorId)
  if (n :: Integer) == 1
    then return $ Right ()
    else return $ Left authorNotExist

getByPostId :: PostId -> Connection -> IO AuthorId
getByPostId postId conn = do
  [Only authorId] <-
    query
      conn
      "SELECT author_id FROM posts \
      \WHERE post_id = ?"
      (Only postId)
  return authorId

getByDraftId :: DraftId -> Connection -> IO AuthorId
getByDraftId draftId conn = do
  [Only authorId] <-
    query
      conn
      "SELECT author_id FROM drafts \
      \WHERE draft_id = ?"
      (Only draftId)
  return authorId

getByToken :: Token -> Connection -> IO AuthorId
getByToken token conn = do
  [Only authorId] <-
    query
      conn
      "SELECT author_id FROM authors a INNER JOIN \
      \users u ON a.user_id = u.user_id WHERE u.token = ?"
      (Only token)
  return authorId

isAuthor :: Token -> Connection -> IO Bool
isAuthor token conn = do
  [Only n] <-
    query
      conn
      "SELECT COUNT(author_id) FROM authors a \
      \INNER JOIN users u ON a.user_id = u.user_id WHERE token = ?"
      (Only token)
  return $ (n :: Integer) == 1
