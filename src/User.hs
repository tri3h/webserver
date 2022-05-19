{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module User where

import Control.Monad (void)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.User as Db
import Error (Error)
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
import Types.Image (Image)
import Types.User (Admin (Admin), CreateUser (..), Login (Login), Name (Name), Password (Password), Surname (Surname), Token (..), UserId (UserId))
import Utility (getImage, getInteger, getMaybeImage, getMaybeText, getText)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> ByteString -> IO Response
create logger pool query body = do
  let cAvatar = getMaybeAvatar body
  let info = do
        cName <- getName query
        cSurname <- getSurname query
        cLogin <- getLogin query
        cPassword <- getPassword query
        Right $
          CreateUser
            { ..
            }
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return . responseLBS status400 [] $ encode l
    Right user -> do
      result <- Handler.create (handle pool) user $ Admin False
      Logger.debug logger $ "Tried to create user and got: " ++ show result
      case result of
        Left l -> return . responseLBS status400 [] $ encode l
        Right token ->
          return . responseLBS status201 [(hContentType, "application/json")] $ encode token

get :: Logger.Handle IO -> Pool Connection -> ServerAddress -> Token -> IO Response
get logger pool server token = do
  result <- Handler.get (handle pool) server token
  Logger.debug logger $ "Tried to get user and got: " ++ show result
  case result of
    Left l -> return . responseLBS status400 [] $ encode l
    Right user ->
      return
        . responseLBS
          status200
          [(hContentType, "application/json")]
        $ encode user

delete :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
delete logger pool query = do
  let info = getUserId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right userId -> do
      result <- Handler.hDelete (handle pool) userId
      Logger.debug logger $ "Tried to delete user and got: " ++ show result
      case result of
        Left l ->
          return
            . responseLBS
              status400
              []
            $ encode l
        Right _ -> return $ responseLBS status204 [] ""
    Left l -> return . responseLBS status400 [] $ encode l

getNewToken :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
getNewToken logger pool query = do
  let info = do
        login <- getLogin query
        password <- getPassword query
        Right (login, password)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (login, password) -> do
      result <- Handler.getNewToken (handle pool) login password
      Logger.debug logger $ "Tried to get new token and got: " ++ show result
      case result of
        Left l -> return . responseLBS status400 [] $ encode l
        Right token ->
          return . responseLBS status201 [(hContentType, "application/json")] $ encode token
    Left l -> return . responseLBS status400 [] $ encode l

addAvatar :: Pool Connection -> Token -> ByteString -> IO Response
addAvatar pool token body = do
  let avatar = getAvatar body
  case avatar of
    Right r -> do
      void . withResource pool $ Db.addAvatar token r
      return $ responseLBS status201 [] ""
    Left l -> return . responseLBS status400 [] $ encode l

makeAdmin :: Pool Connection -> CreateUser -> IO (Either Error Token)
makeAdmin pool admin = Handler.create (handle pool) admin $ Admin True

getName :: QueryText -> Either Error Name
getName query = Name <$> getText query "name"

getToken :: QueryText -> Maybe Token
getToken query = Token <$> getMaybeText query "token"

getUserId :: QueryText -> Either Error UserId
getUserId query = UserId <$> getInteger query "user_id"

getSurname :: QueryText -> Either Error Surname
getSurname query = Surname <$> getText query "surname"

getLogin :: QueryText -> Either Error Login
getLogin query = Login <$> getText query "login"

getPassword :: QueryText -> Either Error Password
getPassword query = Password <$> getText query "password"

getMaybeAvatar :: ByteString -> Maybe Image
getMaybeAvatar body = getMaybeImage body "avatar"

getAvatar :: ByteString -> Either Error Image
getAvatar body = getImage body "avatar"

isAdmin :: Pool Connection -> Handler.Token -> IO Bool
isAdmin pool = withResource pool . Db.isAdmin

isTokenValid :: Pool Connection -> Handler.Token -> IO Bool
isTokenValid pool = withResource pool . Db.isTokenValid

handle :: Pool Connection -> Handler.Handle IO
handle pool =
  let f = withResource pool
   in Handler.Handle
        { Handler.hIsLoginValid = f . Db.isLoginValid,
          Handler.hIsTokenUnique = f . Db.isTokenUnique,
          Handler.hCreate = f . Db.create,
          Handler.hGet = \a b -> f $ Db.get a b,
          Handler.hDelete = f . Db.delete,
          Handler.hGetRandomNumber = randomIO,
          Handler.hFindPassword = f . Db.findPassword,
          Handler.hUpdateToken = \a b -> f $ Db.updateToken a b
        }
