{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module User where

import Network.Wai ( Response, responseLBS )
import Network.HTTP.Types.Status
    ( status200, status201, status204, status400 )
import Network.HTTP.Types.Header ( hContentType )
import Network.HTTP.Types.URI ( QueryText )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text.Encoding ( decodeUtf8)
import qualified Data.Text.Lazy as LazyText
import Data.Text ( Text, unpack )
import Data.ByteString.Lazy ( append )
import Data.Aeson ( encode )
import System.Random ( randomIO )
import Data.Time.Clock ( getCurrentTime )
import Types.User
    ( User(UserToCreate, name, surname, login, password, avatar) )
import qualified Handlers.User as Handler
import qualified Database.Queries.User as Db
import Database.Connection ( manage )
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Types.Image ( Image(Image) )
import Utility ( getText, getInteger, getImage )
import Data.ByteString(ByteString)
import qualified Handlers.Logger as Logger

create :: Logger.Handle IO -> QueryText -> ByteString -> IO Response
create logger query body = do
    let info = do 
            name <- getText query "name"
            surname <- getText query "surname"
            login <- getText query "login"
            password <- getText query "password"
            imageType <- getText query "image_type"
            image <- getImage (decodeUtf8 body) "avatar"
            Right $ UserToCreate {
                avatar = Image image imageType,
                ..
                }
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of 
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right user -> do
            result <- Handler.create handle user
            Logger.debug logger $ "Tried to create user and got: " ++ show result
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right token -> do
                    let a = "{ \"token\" : \"" `append` encodeUtf8 (LazyText.fromStrict token) `append` "\"}"
                    return $ responseLBS status201 [(hContentType, "application/json")] a

get :: Logger.Handle IO -> QueryText -> Text -> IO Response
get logger query token = do
    result <- Handler.get handle token
    Logger.debug logger $ "Tried to get user and got: " ++ show result
    case result of 
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right user -> return $ responseLBS status200 
            [(hContentType, "application/json")] $ encode user

delete :: Logger.Handle IO -> QueryText -> IO Response
delete logger query = do
    let info = getInteger query "user_id"
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of 
        Right userId -> do
            result <- Handler.delete handle userId
            Logger.debug logger $ "Tried to delete user and got: " ++ show result
            case result of
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
                Right _ -> return $ responseLBS status204 [] ""
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

getNewToken :: Logger.Handle IO -> QueryText -> IO Response
getNewToken logger query = do
    let info = do 
            login <- getText query "login"
            password <- getText query "password"
            Right (login, password)
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of 
        Right (login, password) -> do
            result <- Handler.getNewToken handle login password
            Logger.debug logger $ "Tried to get new token and got: " ++ show result
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> do
                    let a = "{ \"token\" : \"" `append` encodeUtf8 (LazyText.fromStrict r) `append` "\"}"
                    return $ responseLBS status201 [(hContentType, "application/json")] a
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

isAdmin :: Handler.Token -> IO Bool
isAdmin = manage . Db.isAdmin

isTokenValid :: Handler.Token -> IO Bool
isTokenValid = manage . Db.isTokenValid

handle :: Handler.Handle IO
handle = Handler.Handle {
    Handler.hIsLoginUnique= manage . Db.isLoginUnique,
    Handler.hIsTokenUnique = manage . Db.isTokenUnique,
    Handler.hIsLoginValid = manage . Db.isLoginValid,
    Handler.hCreate = manage . Db.create,
    Handler.hGet = manage . Db.get,
    Handler.hDelete = manage . Db.delete,
    Handler.hGetRandomNumber = randomIO,
    Handler.hGetCurrentTime = do
        show <$> getCurrentTime,
    Handler.hFindPassword = manage . Db.findPassword,
    Handler.hUpdateToken = \ a b -> manage $ Db.updateToken a b,
    Handler.hDoesExist = manage . Db.doesExist
}