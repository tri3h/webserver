{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module User where

import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.Pool (Pool, withResource)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.User as Db
import qualified Handlers.Logger as Logger
import qualified Handlers.User as Handler
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status
  ( status200,
    status201,
    status204,
    status400,
  )
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response, responseLBS)
import System.Random (randomIO)
import Types.Config (ServerAddress)
import Types.Image (Image (Image))
import Types.User (CreateUser (..), Login (Login), Name (Name), Password (Password), Surname (Surname), Token, UserId (UserId))
import Utility (getImageBody, getInteger, getText)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> ByteString -> IO Response
create logger pool query body = do
  let info = do
        cName <- Name <$> getText query "name"
        cSurname <- Surname <$> getText query "surname"
        cLogin <- Login <$> getText query "login"
        cPassword <- Password <$> getText query "password"
        imageType <- getText query "image_type"
        image <- getImageBody (decodeUtf8 body) "avatar"
        Right $
          CreateUser
            { cAvatar = Image image imageType,
              ..
            }
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right user -> do
      result <- Handler.create (handle pool) user
      Logger.debug logger $ "Tried to create user and got: " ++ show result
      case result of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right token ->
          return $ responseLBS status201 [(hContentType, "application/json")] $ encode token

get :: Logger.Handle IO -> Pool Connection -> ServerAddress -> Token -> IO Response
get logger pool server token = do
  result <- Handler.get (handle pool) server token
  Logger.debug logger $ "Tried to get user and got: " ++ show result
  case result of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right user ->
      return $
        responseLBS
          status200
          [(hContentType, "application/json")]
          $ encode user

delete :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
delete logger pool query = do
  let info = getInteger query "user_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right userId -> do
      result <- Handler.delete (handle pool) $ UserId userId
      Logger.debug logger $ "Tried to delete user and got: " ++ show result
      case result of
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
        Right _ -> return $ responseLBS status204 [] ""
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

getNewToken :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
getNewToken logger pool query = do
  let info = do
        login <- getText query "login"
        password <- getText query "password"
        Right (Login login, Password password)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (login, password) -> do
      result <- Handler.getNewToken (handle pool) login password
      Logger.debug logger $ "Tried to get new token and got: " ++ show result
      case result of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right token ->
          return $ responseLBS status201 [(hContentType, "application/json")] $ encode token
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

isAdmin :: Pool Connection -> Handler.Token -> IO Bool
isAdmin pool = withResource pool . Db.isAdmin

isTokenValid :: Pool Connection -> Handler.Token -> IO Bool
isTokenValid pool = withResource pool . Db.isTokenValid

handle :: Pool Connection -> Handler.Handle IO
handle pool =
  Handler.Handle
    { Handler.hIsLoginUnique = withResource pool . Db.isLoginUnique,
      Handler.hIsTokenUnique = withResource pool . Db.isTokenUnique,
      Handler.hIsLoginValid = withResource pool . Db.isLoginValid,
      Handler.hCreate = withResource pool . Db.create,
      Handler.hGet = withResource pool . Db.get,
      Handler.hDelete = withResource pool . Db.delete,
      Handler.hGetRandomNumber = randomIO,
      Handler.hGetCurrentTime = show <$> getCurrentTime,
      Handler.hFindPassword = withResource pool . Db.findPassword,
      Handler.hUpdateToken = \a b -> withResource pool $ Db.updateToken a b,
      Handler.hDoesExist = withResource pool . Db.doesExist
    }
