{-# LANGUAGE OverloadedStrings #-}
module Database.Queries.User where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time (Date)
import Data.Text
import Types.User
import Types.Image
import Database.Connection


isLoginUnique :: Login -> Connection ->  IO Bool
isLoginUnique login conn = do
    [Only n] <- query conn "SELECT COUNT(login) FROM users WHERE users.login = ?" (Only login)
    return $ (n :: Integer) == 0

isTokenUnique :: Token -> Connection-> IO Bool
isTokenUnique token conn = do
    [Only n] <- query conn "SELECT COUNT(token) FROM users WHERE users.token = ?" (Only token)
    return $ (n :: Integer) == 0

isTokenValid :: Token -> Connection -> IO Bool
isTokenValid token conn = do
    [Only n] <- query conn "SELECT COUNT(token) FROM users WHERE users.token = ?" (Only token)
    return $ (n :: Integer) == 1

isLoginValid :: Login -> Connection -> IO Bool
isLoginValid login conn = do
    [Only n] <- query conn "SELECT COUNT(login) FROM users WHERE users.login = ?" (Only login)
    return $ (n :: Integer) == 1

isAdmin :: Token -> Connection -> IO Bool
isAdmin token conn = do
    [Only admin] <- query conn "SELECT admin FROM users WHERE users.token = ?" (Only token)
    return admin

create :: User -> Connection -> IO Bool
create user conn = do
    [Only imageId] <- query conn "INSERT INTO images (image) VALUES (decode(?,'base64')) \
    \RETURNING image_id" (Only $ avatar user)
    n <- execute conn "INSERT INTO users (name, surname, image_id, \
    \login, password, registration_date, admin, token) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (name user, surname user, imageId :: Integer, login user, password user, date user, admin user, token user)
    return $ n == 1

delete :: UserId -> Connection -> IO Bool
delete userId conn = do
    n <- execute conn "DELETE FROM users WHERE users.user_id = ?" (Only userId)
    return $ n == 1

get :: Token -> Connection -> IO User
get token conn = do
    server <- serverAddress
    [(userId, name, surname, imageId, login, regDate)] <- query conn
        "SELECT user_id, name, surname, image_id, login, registration_date FROM users \
        \ WHERE users.token = ?" (Only token)
    return UserToGet { userId = userId,
                name = name,
                surname = surname,
                avatar = Link $ server `append` "/images?image_id=" `append` pack (show (imageId :: Integer)),
                login = login,
                date = pack $ show (regDate :: Date)}

getByUserId :: UserId -> Connection -> IO User
getByUserId userId conn = do 
    server <- serverAddress
    [(userId, name, surname, imageId, login, regDate)] <- query conn
        "SELECT user_id, name, surname, image_id, login, registration_date FROM users \
        \ WHERE users.user_id = ?" (Only userId)
    return UserToGet { userId = userId,
                name = name,
                surname = surname,
                avatar = Link $ server `append` "/images?image_id=" `append` pack (show (imageId :: Integer)),
                login = login,
                date = pack $ show (regDate :: Date)}

doesExist :: UserId -> Connection -> IO Bool
doesExist userId conn = do
    [Only n] <- query conn "SELECT COUNT(user_id) FROM users WHERE users.user_id = ?" (Only userId)
    return $ (n :: Integer) == 1

findPassword :: Login -> Connection -> IO Password
findPassword login conn = do
    [Only password] <- query conn
        "SELECT password FROM users WHERE users.login = ?" (Only login)
    return password

updateToken :: Login -> Token -> Connection -> IO Bool
updateToken login token conn = do
    n <- execute conn
        "UPDATE users SET token = ? WHERE login = ?" (token, login)
    return $ n == 1