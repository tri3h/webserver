{-# LANGUAGE OverloadedStrings #-}
module User where

import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text.Encoding ( decodeUtf8, decodeUtf16BE )
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
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Types.Image
import Utility
import Data.ByteString(ByteString)

createUser :: QueryText -> ByteString -> IO Response
createUser query body = do
    let isUser = getText query "name" >>= 
            \name -> getText query "surname" >>=
            \surname -> getText query "login" >>=
            \login -> getText query "password" >>=
            \password -> getImage body >>=
            \avatar -> Right $ UserToCreate {
                name = name,
                surname = surname,
                login = login,
                password = password,
                avatar = avatar
                }
    case isUser of 
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right user -> do
                    result <- Handler.createUser handle user
                    case result of
                        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                        Right r -> do
                            let a = "{ \"token\" : \"" `append` encodeUtf8 (LazyText.fromStrict r) `append` "\"}"
                            return $ responseLBS status200 [(hContentType, "application/json")] a

getUser :: QueryText -> Text -> IO Response
getUser query token = do
            user <- Handler.getUser handle token
            return $ responseLBS status200 [(hContentType, "application/json")] $ encode user

deleteUser :: QueryText -> IO Response
deleteUser query = do
    let isUserId = getInteger query "user_id" >>= 
            \userId -> Right userId
    case isUserId of 
        Right userId -> do
            result <- Handler.deleteUser handle userId
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> return $ responseLBS status204 [] ""
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

getNewToken :: QueryText -> IO Response
getNewToken query = do
    let isData = getText query "login" >>= 
            \login -> getText query "password" >>=
            \password -> Right (login, password)
    case isData of 
        Right (login, password) -> do
            result <- Handler.getNewToken handle  login password
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> do
                    let a = "{ \"token\" : \"" `append` encodeUtf8 (LazyText.fromStrict r) `append` "\"}"
                    return $ responseLBS status200 [(hContentType, "application/json")] a
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

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