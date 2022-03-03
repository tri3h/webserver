{-# LANGUAGE OverloadedStrings #-}
module Handler.User(Login, Token, UserId, Handle(..), createUser, deleteUser, getNewToken) where

import Data.Text ( Text, pack, unpack )
import Types.User
import Crypto.Hash
import Data.Text.Encoding 
import qualified Data.ByteString.Char8 as Char8

data Handle m = Handle {
    isLoginUnique :: Login -> m Bool,
    isTokenUnique :: Token -> m Bool,
    create :: User -> m Bool,
    getUser :: Token -> m User,
    delete :: UserId -> m Bool,
    getRandomNumber :: m Integer,
    getCurrentTime :: m String,
    isLoginValid :: Login -> m Bool,
    findPassword :: Login -> m Password,
    updateToken :: Login -> Token -> m Bool
}

hashPassword :: Password -> Password
hashPassword p = pack . show . hashWith SHA256 $ encodeUtf8 p

createUser :: Monad m => Handle m -> User -> m (Either Text Text)
createUser handle recUser = do 
    isUnique <- isLoginUnique handle $ login recUser
    if isUnique 
        then do 
            token <- generateToken handle
            time <- getCurrentTime handle
            let date = pack $ take 10 time
            let hashPassw = hashPassword $ password recUser
            let user = FullUser {
                name = name recUser,
                surname = surname recUser,
                avatar = avatar recUser,
                login = login recUser,
                password = hashPassw,
                date = date,
                admin = False,
                token = token
            }
            isCreated <- create handle user
            if isCreated
                then return $ Right token
                else return $ Left "Failed to create user"
        else return $ Left "Login is already taken"

deleteUser :: Monad m => Handle m -> UserId -> m (Either Text ())
deleteUser handle user_id = do 
    result <- delete handle user_id
    if result 
        then return $ Right ()
        else return $ Left "Failed to delete user"

getNewToken :: Monad m => Handle m -> Password -> Login -> m (Either Text Token)
getNewToken handle login password = do
    isValid <- isLoginValid handle login
    if isValid 
        then do 
            oldPass <- findPassword handle login
            let hash = hashPassword password
            if hash == oldPass
                then do
                    newToken <- generateToken handle
                    updateToken handle login newToken
                    return $ Right newToken
                else return $ Left "Invalid data"
        else return $ Left "Invalid data"

generateToken :: Monad m => Handle m -> m Text
generateToken handle = do 
    number <- getRandomNumber handle
    let token = pack . show . hashWith SHA256 . Char8.pack $ show number
    isUnique <- isTokenUnique handle token
    if isUnique
        then return token
        else generateToken handle