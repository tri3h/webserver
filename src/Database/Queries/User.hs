{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.User where

import Data.Text (Text, pack)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    execute,
    query,
  )
import Database.PostgreSQL.Simple.Time (Date)
import Types.Image (Image (Id, Image))
import Types.User
  ( Login,
    Password,
    Token,
    User
      ( UserToGet,
        admin,
        avatar,
        date,
        login,
        name,
        password,
        surname,
        token,
        userId
      ),
    UserId,
    userNotExist,
  )

isLoginUnique :: Login -> Connection -> IO Bool
isLoginUnique login conn = do
  [Only n] <-
    query
      conn
      "SELECT COUNT(login) FROM users \
      \WHERE users.login = ?"
      (Only login)
  return $ (n :: Integer) == 0

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

create :: User -> Connection -> IO ()
create user conn = do
  let (Image image imageType) = avatar user
  [Only imageId] <-
    query
      conn
      "INSERT INTO images (image, image_type) \
      \VALUES (decode(?,'base64'), ?) \
      \   RETURNING image_id"
      (image, imageType)
  _ <-
    execute
      conn
      "INSERT INTO users (name, surname, image_id, \
      \login, password, registration_date, admin, token) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      ( name user,
        surname user,
        imageId :: Integer,
        login user,
        password user,
        date user,
        admin user,
        token user
      )
  return ()

delete :: UserId -> Connection -> IO ()
delete userId conn = do
  _ <- execute conn "DELETE FROM users WHERE users.user_id = ?" (Only userId)
  return ()

get :: Token -> Connection -> IO User
get token conn = do
  [(userId, name, surname, imageId, login, regDate)] <-
    query
      conn
      "SELECT user_id, name, surname, image_id, login, registration_date \
      \FROM users WHERE users.token = ?"
      (Only token)
  return
    UserToGet
      { avatar = Id imageId,
        date = pack $ show (regDate :: Date),
        ..
      }

getByUserId :: UserId -> Connection -> IO User
getByUserId uId conn = do
  [(userId, name, surname, imageId, login, regDate)] <-
    query
      conn
      "SELECT user_id, name, surname, image_id, login, registration_date \
      \FROM users WHERE users.user_id = ?"
      (Only uId)
  return
    UserToGet
      { avatar = Id imageId,
        date = pack $ show (regDate :: Date),
        ..
      }

getMaybeByUserId :: UserId -> Connection -> IO (Maybe User)
getMaybeByUserId uId conn = do
  x <-
    query
      conn
      "SELECT user_id, name, surname, image_id, login, registration_date \
      \FROM users WHERE users.user_id = ?"
      (Only uId)
  case x of
    [(userId, name, surname, imageId, login, regDate)] ->
      return $
        Just
          UserToGet
            { avatar = Id imageId,
              date = pack $ show (regDate :: Date),
              ..
            }
    _ -> return Nothing

doesExist :: UserId -> Connection -> IO (Either Text ())
doesExist userId conn = do
  [Only n] <-
    query
      conn
      "SELECT COUNT(user_id) FROM users \
      \WHERE users.user_id = ?"
      (Only userId)
  if (n :: Integer) == 1
    then return $ Right ()
    else return $ Left userNotExist

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
