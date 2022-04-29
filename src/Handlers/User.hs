{-# LANGUAGE RecordWildCards #-}

module Handlers.User (Login, Token, UserId, Handle (..), create, get, delete, getNewToken, generateToken, hashPassword) where

import Crypto.Hash (SHA256 (SHA256), hashWith)
import qualified Data.ByteString.Char8 as Char8
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Types.Config (ServerAddress)
import Types.Image (ImageId, Link)
import Types.User
  ( Admin (Admin),
    CreateUser (..),
    FullUser (..),
    GetUser,
    Login,
    Password (Password),
    Token (Token),
    UserId,
    invalidData,
    loginTaken,
  )
import Utility (imageIdToLink)

data Handle m = Handle
  { hIsLoginUnique :: Login -> m Bool,
    hIsTokenUnique :: Token -> m Bool,
    hCreate :: FullUser -> m (),
    hGet :: Token -> (ImageId -> Link) -> m GetUser,
    hDelete :: UserId -> m (),
    hGetRandomNumber :: m Integer,
    hIsLoginValid :: Login -> m Bool,
    hFindPassword :: Login -> m Password,
    hUpdateToken :: Login -> Token -> m (),
    hDoesExist :: UserId -> m (Either Text ())
  }

create :: Monad m => Handle m -> CreateUser -> m (Either Text Token)
create handle partUser = do
  isUnique <- hIsLoginUnique handle $ cLogin partUser
  if isUnique
    then do
      fToken <- generateToken handle
      let hashPassw = hashPassword $ cPassword partUser
      let user =
            FullUser
              { fName = cName partUser,
                fSurname = cSurname partUser,
                fAvatar = cAvatar partUser,
                fLogin = cLogin partUser,
                fPassword = hashPassw,
                fAdmin = Admin False,
                ..
              }
      hCreate handle user
      return $ Right fToken
    else return $ Left loginTaken

get :: Monad m => Handle m -> ServerAddress -> Token -> m (Either Text GetUser)
get handle server token = Right <$> hGet handle token (imageIdToLink server)

delete :: Monad m => Handle m -> UserId -> m (Either Text ())
delete handle userId = do
  exist <- hDoesExist handle userId
  case exist of
    Right () -> Right <$> hDelete handle userId
    Left l -> return $ Left l

hashPassword :: Password -> Password
hashPassword (Password p) = Password . pack . show . hashWith SHA256 $ encodeUtf8 p

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

generateToken :: Monad m => Handle m -> m Token
generateToken handle = do
  number <- hGetRandomNumber handle
  let token = Token . pack . show . hashWith SHA256 . Char8.pack $ show number
  isUnique <- hIsTokenUnique handle token
  if isUnique
    then return token
    else generateToken handle
