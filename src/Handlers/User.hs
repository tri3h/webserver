{-# LANGUAGE OverloadedStrings #-}

module Handlers.User
  ( Login,
    Token,
    UserId,
    Handle (..),
    create,
    get,
    delete,
    getNewToken,
    addAvatar,
    generateToken,
    hashPassword,
    makeAdmin,
    getToken,
  )
where

import Crypto.Hash (SHA256 (SHA256), hashWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Error (Error, invalidData)
import qualified Handlers.Logger as Logger
import Network.HTTP.Types (QueryText)
import Network.Wai (Response)
import Types.Config (ServerAddress)
import Types.Image (Image, ImageId, Link)
import Types.User
  ( Admin (Admin),
    CreateUser (..),
    FullUser (..),
    GetUser,
    Login (Login),
    Name (Name),
    Password (Password),
    Surname (Surname),
    Token (Token),
    UserId (UserId),
  )
import Utility (getImage, getInteger, getMaybeImage, getMaybeText, getText, imageIdToLink, response200JSON, response201, response204, response400)

data Handle m = Handle
  { hCreate :: FullUser -> m (Either Error Token),
    hGet :: Token -> (ImageId -> Link) -> m GetUser,
    hGetRandomNumber :: m Integer,
    hIsLoginValid :: Login -> m Bool,
    hIsTokenUnique :: Token -> m Bool,
    hFindPassword :: Login -> m Password,
    hUpdateToken :: Login -> Token -> m (),
    hDelete :: UserId -> m (Either Error ()),
    hAddAvatar :: Token -> Image -> m (),
    hIsAdmin :: Token -> m Bool,
    hIsTokenValid :: Token -> m Bool
  }

create :: Monad m => Handle m -> Logger.Handle m -> QueryText -> ByteString -> m Response
create handle logger query body = do
  let avatar = getMaybeAvatar body
  let info = do
        name <- getName query
        surname <- getSurname query
        login <- getLogin query
        password <- getPassword query
        Right $
          CreateUser
            { cName = name,
              cAvatar = avatar,
              cSurname = surname,
              cLogin = login,
              cPassword = password
            }
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right user -> do
      result <- createUser handle user $ Admin False
      Logger.debug logger $ "Tried to create user and got: " ++ show result
      case result of
        Left l -> return $ response400 l
        Right token ->
          return $ response200JSON token

createUser :: Monad m => Handle m -> CreateUser -> Admin -> m (Either Error Token)
createUser handle partUser admin = do
  token <- generateToken handle
  let hashPassw = hashPassword $ cPassword partUser
  let user =
        FullUser
          { fName = cName partUser,
            fSurname = cSurname partUser,
            fAvatar = cAvatar partUser,
            fLogin = cLogin partUser,
            fPassword = hashPassw,
            fToken = token,
            fAdmin = admin
          }
  hCreate handle user

get :: Monad m => Handle m -> Logger.Handle m -> ServerAddress -> Token -> m Response
get handle logger server token = do
  result <- hGet handle token (imageIdToLink server)
  Logger.debug logger $ "Tried to get user and got: " ++ show result
  return $ response200JSON result

delete :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
delete handle logger query = do
  let info = getUserId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right userId -> do
      result <- hDelete handle userId
      Logger.debug logger $ "Tried to delete user and got: " ++ show result
      case result of
        Left l -> return $ response400 l
        Right _ -> return response204
    Left l -> return $ response400 l

getNewToken :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
getNewToken handle logger query = do
  let info = do
        login <- getLogin query
        password <- getPassword query
        Right (login, password)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (login, password) -> do
      result <- makeNewToken handle login password
      Logger.debug logger $ "Tried to get new token and got: " ++ show result
      case result of
        Left l -> return $ response400 l
        Right token ->
          return $ response200JSON token
    Left l -> return $ response400 l

makeNewToken :: Monad m => Handle m -> Login -> Password -> m (Either Error Token)
makeNewToken handle login password = do
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

addAvatar :: Monad m => Handle m -> Token -> ByteString -> m Response
addAvatar handle token body = do
  let avatar = getAvatar body
  case avatar of
    Right r -> do
      hAddAvatar handle token r
      return response201
    Left l -> return $ response400 l

makeAdmin :: Monad m => Handle m -> CreateUser -> m (Either Error Token)
makeAdmin handle admin = createUser handle admin $ Admin True

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

hashPassword :: Password -> Password
hashPassword (Password p) = Password . pack . show . hashWith SHA256 $ encodeUtf8 p

generateToken :: Monad m => Handle m -> m Token
generateToken handle = do
  number <- hGetRandomNumber handle
  let token = Token . pack . show . hashWith SHA256 . Char8.pack $ show number
  isUnique <- hIsTokenUnique handle token
  if isUnique
    then return token
    else generateToken handle
