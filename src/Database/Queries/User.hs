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
import qualified Database.PostgreSQL.Simple.Time as Time
import Types.Image (Image (Id, Image))
import Types.User
  ( Date (Date),
    FullUser (..),
    GetUser (..),
    Login,
    Password,
    Token,
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

create :: FullUser -> Connection -> IO ()
create user conn = do
  let (Image image imageType) = fAvatar user
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
      ( fName user,
        fSurname user,
        imageId :: Integer,
        fLogin user,
        fPassword user,
        fDate user,
        fAdmin user,
        fToken user
      )
  return ()

delete :: UserId -> Connection -> IO ()
delete userId conn = do
  _ <- execute conn "DELETE FROM users WHERE users.user_id = ?" (Only userId)
  return ()

get :: Token -> Connection -> IO GetUser
get token conn = do
  [(gUserId, gName, gSurname, imageId, gLogin, regDate)] <-
    query
      conn
      "SELECT user_id, name, surname, image_id, login, registration_date \
      \FROM users WHERE users.token = ?"
      (Only token)
  return
    GetUser
      { gAvatar = Id imageId,
        gDate = Date . pack $ show (regDate :: Time.Date),
        ..
      }

getByUserId :: UserId -> Connection -> IO GetUser
getByUserId uId conn = do
  [(gUserId, gName, gSurname, imageId, gLogin, regDate)] <-
    query
      conn
      "SELECT user_id, name, surname, image_id, login, registration_date \
      \FROM users WHERE users.user_id = ?"
      (Only uId)
  return
    GetUser
      { gAvatar = Id imageId,
        gDate = Date . pack $ show (regDate :: Time.Date),
        ..
      }

getMaybeByUserId :: UserId -> Connection -> IO (Maybe GetUser)
getMaybeByUserId uId conn = do
  x <-
    query
      conn
      "SELECT user_id, name, surname, image_id, login, registration_date \
      \FROM users WHERE users.user_id = ?"
      (Only uId)
  case x of
    [(gUserId, gName, gSurname, imageId, gLogin, regDate)] ->
      return $
        Just
          GetUser
            { gAvatar = Id imageId,
              gDate = Date . pack $ show (regDate :: Time.Date),
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
