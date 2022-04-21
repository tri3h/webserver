{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module User where

import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (append)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Database.Connection (manage)
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
import Types.Config (Config (database, server), DatabaseConfig, ServerConfig (sAddress))
import Types.Image (Image (Image))
import Types.User
  ( User (UserToCreate, avatar, login, name, password, surname),
  )
import Utility (getImage, getInteger, getText)

create :: Logger.Handle IO -> Config -> QueryText -> ByteString -> IO Response
create logger config query body = do
  let info = do
        name <- getText query "name"
        surname <- getText query "surname"
        login <- getText query "login"
        password <- getText query "password"
        imageType <- getText query "image_type"
        image <- getImage (decodeUtf8 body) "avatar"
        Right $
          UserToCreate
            { avatar = Image image imageType,
              ..
            }
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right user -> do
      result <- Handler.create (handle config) user
      Logger.debug logger $ "Tried to create user and got: " ++ show result
      case result of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right token -> do
          let a = "{ \"token\" : \"" `append` encodeUtf8 (LazyText.fromStrict token) `append` "\"}"
          return $ responseLBS status201 [(hContentType, "application/json")] a

get :: Logger.Handle IO -> Config -> Text -> IO Response
get logger config token = do
  result <- Handler.get (handle config) (sAddress $ server config) token
  Logger.debug logger $ "Tried to get user and got: " ++ show result
  case result of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right user ->
      return $
        responseLBS
          status200
          [(hContentType, "application/json")]
          $ encode user

delete :: Logger.Handle IO -> Config -> QueryText -> IO Response
delete logger config query = do
  let info = getInteger query "user_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right userId -> do
      result <- Handler.delete (handle config) userId
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

getNewToken :: Logger.Handle IO -> Config -> QueryText -> IO Response
getNewToken logger config query = do
  let info = do
        login <- getText query "login"
        password <- getText query "password"
        Right (login, password)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (login, password) -> do
      result <- Handler.getNewToken (handle config) login password
      Logger.debug logger $ "Tried to get new token and got: " ++ show result
      case result of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right r -> do
          let a = "{ \"token\" : \"" `append` encodeUtf8 (LazyText.fromStrict r) `append` "\"}"
          return $ responseLBS status201 [(hContentType, "application/json")] a
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

isAdmin :: DatabaseConfig -> Handler.Token -> IO Bool
isAdmin config = manage config . Db.isAdmin

isTokenValid :: DatabaseConfig -> Handler.Token -> IO Bool
isTokenValid config = manage config . Db.isTokenValid

handle :: Config -> Handler.Handle IO
handle config =
  let db = database config
   in Handler.Handle
        { Handler.hIsLoginUnique = manage db . Db.isLoginUnique,
          Handler.hIsTokenUnique = manage db . Db.isTokenUnique,
          Handler.hIsLoginValid = manage db . Db.isLoginValid,
          Handler.hCreate = manage db . Db.create,
          Handler.hGet = manage db . Db.get,
          Handler.hDelete = manage db . Db.delete,
          Handler.hGetRandomNumber = randomIO,
          Handler.hGetCurrentTime = show <$> getCurrentTime,
          Handler.hFindPassword = manage db . Db.findPassword,
          Handler.hUpdateToken = \a b -> manage db $ Db.updateToken a b,
          Handler.hDoesExist = manage db . Db.doesExist
        }
