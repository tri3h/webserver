{-# LANGUAGE OverloadedStrings #-}
module User where

import Data.Text ( Text, pack, unpack )
import qualified Database.User as Db
import qualified Types.User.FullUser as F
import qualified Types.User.ReceivedUser as R
import qualified Types.User.SentUser as S
import Data.Time.Clock
import Crypto.Hash
import Data.Text.Encoding 
import System.Random
import qualified Data.ByteString.Char8 as Char8

create :: R.ReceivedUser -> IO (Either Text Text)
create recUser = do 
    isUnique <- Db.isLoginUnique $ R.login recUser
    if isUnique 
        then do 
            token <- generateToken
            time <- getCurrentTime
            let date = pack . take 10 $ show time
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
            isCreated <- Db.create user
            if isCreated
                then return $ Right token
                else return $ Left "Failed to create user"
        else return $ Left "Login is already taken"

get :: Text -> IO S.SentUser
get = Db.findByToken

delete :: Text -> IO (Either Text ())
delete user_id = do 
    result <- Db.delete (read $ unpack user_id :: Integer)
    if result 
        then return $ Right ()
        else return $ Left "Failed to delete user"

generateToken :: IO Text
generateToken = do 
    number <- (randomIO :: IO Integer) 
    let token = pack . show . hashWith SHA256 . Char8.pack $ show number
    isUnique <- Db.isTokenUnique token
    if isUnique
        then return token
        else generateToken

isTokenValid :: Text -> IO Bool
isTokenValid = Db.isTokenValid

isAdmin :: Text -> IO Bool 
isAdmin = Db.isAdmin 
