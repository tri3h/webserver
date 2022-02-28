{-# LANGUAGE OverloadedStrings #-}
module Handler.User(Login, Token, UserID, Handle(..), createUser, deleteUser) where

import Data.Text ( Text, pack, unpack )
import qualified Types.User.FullUser as F
import qualified Types.User.ReceivedUser as R
import qualified Types.User.SentUser as S
import Crypto.Hash
import Data.Text.Encoding 
import qualified Data.ByteString.Char8 as Char8

type Login = Text
type Token = Text 
type UserID = Integer

data Handle m = Handle {
    isLoginUnique :: Login -> m Bool,
    isTokenUnique :: Token -> m Bool,
    create :: F.FullUser -> m Bool,
    getUser :: Token -> m S.SentUser,
    delete :: UserID -> m Bool,
    getRandomNumber :: m Integer,
    getCurrentTime :: m String
}

createUser :: Monad m => Handle m -> R.ReceivedUser -> m (Either Text Text)
createUser handle recUser = do 
    isUnique <- isLoginUnique handle $ R.login recUser
    if isUnique 
        then do 
            token <- generateToken handle
            time <- getCurrentTime handle
            let date = pack $ take 10 time
            let hashPassw = pack . show . hashWith SHA256 . encodeUtf8 $ R.password recUser
            let user = F.FullUser {
                F.name = R.name recUser,
                F.surname = R.surname recUser,
                F.avatar = R.avatar recUser,
                F.login = R.login recUser,
                F.password = hashPassw,
                F.date = date,
                F.admin = False,
                F.token = token
            }
            isCreated <- create handle user
            if isCreated
                then return $ Right token
                else return $ Left "Failed to create user"
        else return $ Left "Login is already taken"

deleteUser :: Monad m => Handle m -> UserID -> m (Either Text ())
deleteUser handle user_id = do 
    result <- delete handle user_id
    if result 
        then return $ Right ()
        else return $ Left "Failed to delete user"

generateToken :: Monad m => Handle m -> m Text
generateToken handle = do 
    number <- getRandomNumber handle
    let token = pack . show . hashWith SHA256 . Char8.pack $ show number
    isUnique <- isTokenUnique handle token
    if isUnique
        then return token
        else generateToken handle