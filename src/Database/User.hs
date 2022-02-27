{-# LANGUAGE OverloadedStrings #-}
module Database.User where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time (Date)
import Data.Text
import Control.Monad
import qualified Types.User.SentUser as S
import qualified Types.User.FullUser as F

--openConnection :: IO Connection
--openConnection = connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}

--closeConnection :: Connection -> IO ()
--closeConnection = close

isLoginUnique :: Text -> IO Bool
isLoginUnique login = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [Only n] <- query conn "SELECT COUNT(login) FROM users WHERE users.login = ?" (Only login)
    close conn
    return $ (n :: Integer) == 0

isTokenUnique :: Text -> IO Bool
isTokenUnique token = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [Only n] <- query conn "SELECT COUNT(token) FROM users WHERE users.token = ?" (Only token)
    close conn
    return $ (n :: Integer) == 0

isTokenValid :: Text -> IO Bool
isTokenValid token = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [Only n] <- query conn "SELECT COUNT(token) FROM users WHERE users.token = ?" (Only token)
    close conn
    return $ (n :: Integer) == 1

isAdmin :: Text -> IO Bool
isAdmin token = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [Only admin] <- query conn "SELECT admin FROM users WHERE users.token = ?" (Only token)
    close conn
    return admin

create :: F.FullUser -> IO Bool
create user = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    n <- execute conn "INSERT INTO users (name, surname, avatar, login, password, registration_date, admin, token) VALUES (?, ?, ?, ?, ?, ?, ?, ?)" 
        (F.name user, F.surname user, F.avatar user, F.login user, F.password user, F.date user, F.admin user, F.token user)
    close conn
    return $ n == 1

delete :: Integer -> IO Bool
delete user_id = do 
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    n <- execute conn "DELETE FROM users WHERE users.user_id = ?" (Only user_id)
    close conn
    return $ n == 1

doesExist :: Integer -> IO Bool
doesExist user_id = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [Only n] <- query conn "SELECT COUNT(user_id) FROM users WHERE users.user_id = ?" (Only user_id)
    close conn
    return $ (n :: Integer) == 1

findByToken :: Text -> IO S.SentUser
findByToken token = do
    conn <- connect defaultConnectInfo {connectDatabase = "webserver", connectPassword = "password"}
    [(name, surname, avatar, login, reg_date)] <- query conn 
        "SELECT name, surname, avatar, login, registration_date FROM users WHERE users.token = ?" (Only token)
    close conn
    let date = pack $ show (reg_date :: Date)
    return S.SentUser {S.name = name,
                S.surname = surname,
                S.avatar = avatar,
                S.login = login,
                S.date = date}