{-# LANGUAGE OverloadedStrings #-}
module User where

import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Text.Lazy as LazyText
import Data.Text ( Text, unpack )
import Control.Monad
import Data.ByteString.Lazy ( append )
import Data.Aeson
import System.Random
import Data.Time.Clock
import Types.User
import qualified Handler.User as Handler
import qualified Database.Queries.User as Db
import Database.Connection

createUser :: Query -> IO Response
createUser query = do
    let name = join $ lookup "name" query
    let surname = join $ lookup "surname" query
    let avatar = join $ lookup "avatar" query
    let login = join $ lookup "login" query
    let password = join $ lookup "password" query
    let arr = [name, surname, avatar, login, password]
    if Nothing `notElem` arr
        then do
            let val' = map (decodeUtf8 . (\(Just a) -> a)) arr
            let user = CreateUser {
                    name = head val',
                    surname = val' !! 1,
                    avatar = val' !! 2,
                    login = val' !! 3,
                    password = val' !! 4
            }
            result <- Handler.createUser handle user
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> do
                    let a = "{ \"token\" : \"" `append` encodeUtf8 (LazyText.fromStrict r) `append` "\"}"
                    return $ responseLBS status200 [(hContentType, "application/json")] a
        else return $ responseLBS status400 [] ""


getUser :: Query -> Text -> IO Response
getUser query token = do
            user <- Handler.getUser handle token
            return $ responseLBS status200 [(hContentType, "application/json")] $ encode user

deleteUser :: Query -> IO Response
deleteUser query = do
        let user_id = join $ lookup "user_id" query
        case user_id of
            Just u -> do
                result <- Handler.deleteUser handle (read . unpack $ decodeUtf8 u :: Integer)
                case result of
                    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                    Right r -> return $ responseLBS status204 [] ""
            Nothing -> return $ responseLBS status400 [] ""

getNewToken :: Query -> IO Response
getNewToken query = do
    let login = join $ lookup "login" query
    let password = join $ lookup "password" query
    let arr = [login, password]
    if Nothing `notElem` arr
        then do
            let val = map (decodeUtf8 . (\(Just a) -> a)) arr
            result <- Handler.getNewToken handle (val !! 0) (val !! 1)
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> do
                    let a = "{ \"token\" : \"" `append` encodeUtf8 (LazyText.fromStrict r) `append` "\"}"
                    return $ responseLBS status200 [(hContentType, "application/json")] a
        else return $ responseLBS status400 [] ""

isAdmin :: Handler.Token -> IO Bool
isAdmin = manage . Db.isAdmin

isTokenValid :: Handler.Token -> IO Bool
isTokenValid = manage . Db.isTokenValid

handle = Handler.Handle {
    Handler.isLoginUnique= manage . Db.isLoginUnique,
    Handler.isTokenUnique = manage . Db.isTokenUnique,
    Handler.create = manage . Db.create,
    Handler.getUser = manage . Db.get,
    Handler.delete = manage . Db.delete,
    Handler.getRandomNumber = randomIO,
    Handler.getCurrentTime = do
        show <$> getCurrentTime,
    Handler.isLoginValid = manage . Db.isLoginValid,
    Handler.findPassword = manage . Db.findPassword,
    Handler.updateToken = \ a b -> manage $ Db.updateToken a b 
}