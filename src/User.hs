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
import Utility
import qualified Data.ByteString as BS
import qualified Data.Text as Text

createUser :: Query -> BS.ByteString -> IO Response
createUser query body = do
    let res = queryToList query ["name", "surname", "login", "password"]
    let avatar = getPicture body "avatar"
    case avatar of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right pic -> case res of
                Right r -> do
                    let user = CreateUser {
                        name = head r,
                        surname = r!! 1,
                        login = r !! 2,
                        password = r !! 3,
                        avatar = pic
                        }
                    result <- Handler.createUser handle user
                    case result of
                        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                        Right r -> do
                            let a = "{ \"token\" : \"" `append` encodeUtf8 (LazyText.fromStrict r) `append` "\"}"
                            return $ responseLBS status200 [(hContentType, "application/json")] a
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

getUser :: Query -> Text -> IO Response
getUser query token = do
            user <- Handler.getUser handle token
            return $ responseLBS status200 [(hContentType, "application/json")] $ encode user

getAvatar :: Query -> IO Response
getAvatar query = do
    let res = queryToList query ["picture_id"]
    case res of
            Right r -> do
                result <- manage $ Db.getAvatar (head r)
                let html = "<img src=\"data:image/png;base64," `Text.append` result `Text.append` "\"/>"
                return $ responseLBS status200 [(hContentType, "text/html")] . encodeUtf8 $ LazyText.fromStrict html
            Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

deleteUser :: Query -> IO Response
deleteUser query = do
        let res = queryToList query ["user_id"]
        case res of
            Right r -> do
                result <- Handler.deleteUser handle (read . unpack $ head r :: Integer)
                case result of
                    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                    Right r -> return $ responseLBS status204 [] ""
            Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

getNewToken :: Query -> IO Response
getNewToken query = do
    let res = queryToList query ["login", "password"]
    case res of
        Right r -> do
            result <- Handler.getNewToken handle (head r) (r !! 1)
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