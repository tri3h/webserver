{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.User where

import Control.Exception (try)
import Control.Monad (void)
import Data.Text (pack)
import Database.PostgreSQL.Simple
  ( Binary (Binary),
    Connection,
    Only (Only),
    SqlError (sqlErrorMsg),
    execute,
    query,
    query_,
  )
import qualified Database.PostgreSQL.Simple.Time as Time
import Error (Error, loginTaken, unknownError, userNotExist)
import Types.Image (Image (Image), ImageId, Link)
import Types.User
  ( Date (Date),
    FullUser (..),
    GetUser (..),
    Login,
    Password,
    PostUser (..),
    Token,
    UserId,
  )

isTokenUnique :: Token -> Connection -> IO Bool
isTokenUnique token conn = do
  [Only n] <-
    query
      conn
      "SELECT COUNT(token) FROM users \
      \WHERE users.token = ?"
      (Only token)
  return $ (n :: Integer) == 0

isTokenValid :: Token -> Connection -> IO Bool
isTokenValid token conn = do
  [Only n] <-
    query
      conn
      "SELECT COUNT(token) FROM users \
      \WHERE users.token = ?"
      (Only token)
  return $ (n :: Integer) == 1

isLoginValid :: Login -> Connection -> IO Bool
isLoginValid login conn = do
  [Only n] <-
    query
      conn
      "SELECT COUNT(login) FROM users \
      \WHERE users.login = ?"
      (Only login)
  return $ (n :: Integer) == 1

isAdmin :: Token -> Connection -> IO Bool
isAdmin token conn = do
  [Only admin] <-
    query
      conn
      "SELECT admin FROM users \
      \WHERE users.token = ?"
      (Only token)
  return admin

create :: FullUser -> Connection -> IO (Either Error Token)
create user conn = do
  result <- case fAvatar user of
    Just (Image image imageType) ->
      try . void $
        execute
          conn
          "WITH i AS (INSERT INTO images (image, image_type) \
          \VALUES (?, ?) RETURNING image_id) \
          \INSERT INTO users (name, surname, image_id, \
          \login, password, registration_date, admin, token) \
          \VALUES (?, ?, (SELECT image_id FROM i), ?, ?, CURRENT_DATE, ?, ?)"
          ( Binary image,
            imageType,
            fName user,
            fSurname user,
            fLogin user,
            fPassword user,
            fAdmin user,
            fToken user
          ) ::
        IO (Either SqlError ())
    Nothing ->
      try . void $
        execute
          conn
          "INSERT INTO users (name, surname, login, password, \
          \registration_date, admin, token) VALUES (?, ?, ?, ?, CURRENT_DATE, ?, ?)"
          ( fName user,
            fSurname user,
            fLogin user,
            fPassword user,
            fAdmin user,
            fToken user
          ) ::
        IO (Either SqlError ())
  return $ case result of
    Right _ -> Right $ fToken user
    Left l -> case sqlErrorMsg l of
      "duplicate key value violates unique constraint \"login_unique\"" -> Left loginTaken
      _ -> Left unknownError

delete :: UserId -> Connection -> IO (Either Error ())
delete userId conn = do
  result <- execute conn "DELETE FROM users WHERE users.user_id = ?" (Only userId)
  return $ if result == 0 then Left userNotExist else Right ()

get :: Token -> (ImageId -> Link) -> Connection -> IO GetUser
get token f conn = do
  [(gUserId, gName, gSurname, imageId, gLogin, regDate)] <-
    query
      conn
      "SELECT user_id, name, surname, image_id, login, registration_date \
      \FROM users WHERE users.token = ?"
      (Only token)
  return
    GetUser
      { gAvatar = f <$> imageId,
        gDate = Date . pack $ show (regDate :: Time.Date),
        ..
      }

addAvatar :: Token -> Image -> Connection -> IO ()
addAvatar token (Image image imageType) conn =
  void $
    execute
      conn
      "WITH i AS (INSERT INTO images (image, image_type) \
      \VALUES (?, ?) RETURNING image_id) \
      \UPDATE users SET image_id = (SELECT image_id FROM i) \
      \WHERE token = ?"
      (Binary image, imageType, token)

getPostUser :: UserId -> Connection -> IO (Maybe PostUser)
getPostUser uId conn = do
  x <-
    query
      conn
      "SELECT user_id, name, surname FROM users WHERE users.user_id = ?"
      (Only uId)
  case x of
    [(pUserId, pName, pSurname)] ->
      return $
        Just
          PostUser
            { ..
            }
    _ -> return Nothing

findPassword :: Login -> Connection -> IO Password
findPassword login conn = do
  [Only password] <-
    query
      conn
      "SELECT password FROM users WHERE users.login = ?"
      (Only login)
  return password

updateToken :: Login -> Token -> Connection -> IO ()
updateToken login token conn = do
  _ <-
    execute
      conn
      "UPDATE users SET token = ? WHERE login = ?"
      (token, login)
  return ()

hasAdmin :: Connection -> IO Bool
hasAdmin conn = do
  [Only n] <-
    query_
      conn
      "SELECT COUNT(user_id) FROM users \
      \WHERE admin = true"
  return $ (n :: Integer) > 0
