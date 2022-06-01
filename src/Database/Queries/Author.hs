{-# LANGUAGE OverloadedStrings #-}

module Database.Queries.Author where

import Control.Exception (try)
import Control.Monad (void)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    SqlError (sqlErrorMsg),
    execute,
    query,
  )
import Error (Error, alreadyAuthor, authorNotExist, draftNotExist, unknownError, userNotAuthor, userNotExist)
import Types.Author
  ( AuthorId,
    CreateAuthor (..),
    EditAuthor (..),
    GetAuthor (..),
  )
import Types.Draft (DraftId)
import Types.PostComment (PostId)
import Types.User (Token)

create :: CreateAuthor -> Connection -> IO (Either Error ())
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
    Left l -> case sqlErrorMsg l of
      "duplicate key value violates unique constraint \"user_id_unique\"" -> Left alreadyAuthor
      "insert or update on table \"authors\" violates foreign key constraint \"user_id\"" -> Left userNotExist
      _ -> Left unknownError

delete :: AuthorId -> Connection -> IO (Either Error ())
delete authorId conn = do
  result <- execute conn "DELETE FROM authors WHERE authors.author_id = ?" (Only authorId)
  return $ if result == 0 then Left authorNotExist else Right ()

get :: AuthorId -> Connection -> IO (Either Error GetAuthor)
get authId conn = do
  result <-
    query
      conn
      "SELECT author_id, user_id, description FROM authors \
      \WHERE authors.author_id = ?"
      (Only authId)
  return $
    if null result
      then Left authorNotExist
      else Right $ (\[(authorId, userId, description)] -> GetAuthor {gAuthorId = authorId, gUserId = userId, gDescription = description}) result

getMaybe :: AuthorId -> Connection -> IO (Maybe GetAuthor)
getMaybe authId conn = do
  x <-
    query
      conn
      "SELECT author_id, user_id, description FROM authors \
      \WHERE authors.author_id = ?"
      (Only authId)
  case x of
    [(authorId, userId, description)] ->
      return $ Just GetAuthor {gAuthorId = authorId, gUserId = userId, gDescription = description}
    _ -> return Nothing

edit :: EditAuthor -> Connection -> IO (Either Error ())
edit author conn = do
  result <-
    execute
      conn
      "UPDATE authors SET description = ? \
      \WHERE authors.author_id = ?"
      (eDescription author, eAuthorId author)
  return $ if result == 0 then Left authorNotExist else Right ()

getByPostId :: PostId -> Connection -> IO AuthorId
getByPostId postId conn = do
  [Only authorId] <-
    query
      conn
      "SELECT author_id FROM posts \
      \WHERE post_id = ?"
      (Only postId)
  return authorId

getByDraftId :: DraftId -> Connection -> IO (Either Error AuthorId)
getByDraftId draftId conn = do
  result <-
    query
      conn
      "SELECT author_id FROM drafts \
      \WHERE draft_id = ?"
      (Only draftId)
  return $ case result of
    [Only authorId] -> Right authorId
    _ -> Left draftNotExist

getByToken :: Token -> Connection -> IO (Either Error AuthorId)
getByToken token conn = do
  result <-
    query
      conn
      "SELECT author_id FROM authors a INNER JOIN \
      \users u ON a.user_id = u.user_id WHERE u.token = ?"
      (Only token)
  return $ case result of
    [Only authorId] -> Right authorId
    _ -> Left userNotAuthor
