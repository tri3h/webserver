{-# LANGUAGE OverloadedStrings #-}
module Database.User where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time (Date)
import Data.Text
import Control.Monad
import qualified Types.User.SentUser as S
import Types.User.FullUser

--openConnection :: IO Connection
--openConnection = connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}

--closeConnection :: Connection -> IO ()
--closeConnection = close

isLoginUnique :: Login -> IO Bool
isLoginUnique login = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [Only n] <- query conn "SELECT COUNT(login) FROM users WHERE users.login = ?" (Only login)
    close conn
    return $ (n :: Integer) == 0

isTokenUnique :: Token -> IO Bool
isTokenUnique token = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [Only n] <- query conn "SELECT COUNT(token) FROM users WHERE users.token = ?" (Only token)
    close conn
    return $ (n :: Integer) == 0

isTokenValid :: Token -> IO Bool
isTokenValid token = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [Only n] <- query conn "SELECT COUNT(token) FROM users WHERE users.token = ?" (Only token)
    close conn
    return $ (n :: Integer) == 1

isLoginValid :: Login -> IO Bool
isLoginValid login = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [Only n] <- query conn "SELECT COUNT(login) FROM users WHERE users.login = ?" (Only login)
    close conn
    return $ (n :: Integer) == 1

isAdmin :: Token -> IO Bool
isAdmin token = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [Only admin] <- query conn "SELECT admin FROM users WHERE users.token = ?" (Only token)
    close conn
    return admin

create :: FullUser -> IO Bool
create user = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    n <- execute conn "INSERT INTO users (name, surname, avatar, login, password, registration_date, admin, token) VALUES (?, ?, ?, ?, ?, ?, ?, ?)" 
        (name user, surname user, avatar user, login user, password user, date user, admin user, token user)
    close conn
    return $ n == 1

delete :: UserId -> IO Bool
delete userId = do 
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    n <- execute conn "DELETE FROM users WHERE users.user_id = ?" (Only userId)
    close conn
    return $ n == 1

doesExist :: UserId -> IO Bool
doesExist userId = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [Only n] <- query conn "SELECT COUNT(user_id) FROM users WHERE users.user_id = ?" (Only userId)
    close conn
    return $ (n :: Integer) == 1

findByToken :: Token -> IO S.SentUser
findByToken token = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [(userId, name, surname, avatar, login, reg_date)] <- query conn 
        "SELECT user_id, name, surname, avatar, login, registration_date FROM users WHERE users.token = ?" (Only token)
    close conn
    let date = pack $ show (reg_date :: Date)
    return S.SentUser { S.userId = userId, 
                S.name = name,
                S.surname = surname,
                S.avatar = avatar,
                S.login = login,
                S.date = date}

findPassword :: Login -> IO Password
findPassword login = do 
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [Only password] <- query conn 
        "SELECT password FROM users WHERE users.login = ?" (Only login)
    close conn
    return password

updateToken :: Login -> Token -> IO Bool
updateToken login token = do 
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    n <- execute conn 
        "UPDATE users SET token = ? WHERE login = ?" (token, login)
    close conn
    return $ n == 1