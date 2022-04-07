{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handlers.User(Login, Token, UserId, Handle(..), create, get, delete, getNewToken, generateToken, hashPassword) where

import Data.Text ( Text, pack, unpack )
import Types.User
    ( Password,
      UserId,
      Token,
      Login,
      User(User, password, date, admin, token, name, surname, avatar,
           login, UserToCreate, UserToGet), loginTaken, malformedUser, invalidData )
import Crypto.Hash ( hashWith, SHA256(SHA256) )
import Data.Text.Encoding ( encodeUtf8 )
import qualified Data.ByteString.Char8 as Char8
import Types.Image ( Image(Image, Link), malformedImage )

data Handle m = Handle {
    hIsLoginUnique :: Login -> m Bool,
    hIsTokenUnique :: Token -> m Bool,
    hCreate :: User -> m (),
    hGet :: Token -> m User,
    hDelete :: UserId -> m (),
    hGetRandomNumber :: m Integer,
    hGetCurrentTime :: m String,
    hIsLoginValid :: Login -> m Bool,
    hFindPassword :: Login -> m Password,
    hUpdateToken :: Login -> Token -> m (),
    hDoesExist :: UserId -> m (Either Text ())
}

create :: Monad m => Handle m -> User -> m (Either Text Text)
create handle partUser@UserToCreate {} = do
    isUnique <- hIsLoginUnique handle $ login partUser
    if isUnique
    then do
        case avatar partUser of 
            Link _ -> return $ Left malformedImage
            Image _ _ -> do 
                token <- generateToken handle
                time <- hGetCurrentTime handle
                let date = pack $ take 10 time
                let hashPassw = hashPassword $ password partUser
                let user = User {
                        name = name partUser,
                        surname = surname partUser,
                        avatar = avatar partUser,
                        login = login partUser,
                        password = hashPassw,
                        admin = False,
                        .. }
                hCreate handle user
                return $ Right token
    else return $ Left loginTaken 
create _ _ = return $ Left malformedUser 

get :: Monad m => Handle m -> Token -> m (Either Text User)
get handle token = do 
    user <- hGet handle token
    case user of 
        u@UserToGet {} -> do 
            case avatar user of 
                Link _ -> return $ Right user
                Image _ _ -> return $ Left malformedImage
        _ -> return $ Left malformedUser

delete :: Monad m => Handle m -> UserId -> m (Either Text ())
delete handle userId = do
    exist <- hDoesExist handle userId
    case exist of 
        Right () -> Right <$> hDelete handle userId
        Left l -> return $ Left l 

hashPassword :: Password -> Password
hashPassword p = pack . show . hashWith SHA256 $ encodeUtf8 p

getNewToken :: Monad m => Handle m -> Login -> Password -> m (Either Text Token)
getNewToken handle login password = do
    isValid <- hIsLoginValid handle login
    if isValid
    then do
        oldPass <- hFindPassword handle login
        let isPasswordValid = hashPassword password == oldPass
        if isPasswordValid
        then do
            newToken <- generateToken handle
            hUpdateToken handle login newToken
            return $ Right newToken
        else return $ Left invalidData
    else return $ Left invalidData 

generateToken :: Monad m => Handle m -> m Text
generateToken handle = do
    number <- hGetRandomNumber handle
    let token = pack . show . hashWith SHA256 . Char8.pack $ show number
    isUnique <- hIsTokenUnique handle token
    if isUnique
    then return token
    else generateToken handle