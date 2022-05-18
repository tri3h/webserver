{-# LANGUAGE RecordWildCards #-}

module Handlers.User (Login, Token, UserId, Handle (..), create, get, getNewToken, generateToken, hashPassword) where

import Crypto.Hash (SHA256 (SHA256), hashWith)
import qualified Data.ByteString.Char8 as Char8
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Error (Error, invalidData)
import Types.Config (ServerAddress)
import Types.Image (ImageId, Link)
import Types.User
  ( Admin,
    CreateUser (..),
    FullUser (..),
    GetUser,
    Login,
    Password (Password),
    Token (Token),
    UserId,
  )
import Utility (imageIdToLink)

data Handle m = Handle
  { hCreate :: FullUser -> m (Either Error Token),
    hGet :: Token -> (ImageId -> Link) -> m GetUser,
    hGetRandomNumber :: m Integer,
    hIsLoginValid :: Login -> m Bool,
    hIsTokenUnique :: Token -> m Bool,
    hFindPassword :: Login -> m Password,
    hUpdateToken :: Login -> Token -> m ()
  }

create :: Monad m => Handle m -> CreateUser -> Admin -> m (Either Error Token)
create handle partUser fAdmin = do
  fToken <- generateToken handle
  let hashPassw = hashPassword $ cPassword partUser
  let user =
        FullUser
          { fName = cName partUser,
            fSurname = cSurname partUser,
            fAvatar = cAvatar partUser,
            fLogin = cLogin partUser,
            fPassword = hashPassw,
            ..
          }
  hCreate handle user

get :: Monad m => Handle m -> ServerAddress -> Token -> m (Either Error GetUser)
get handle server token = Right <$> hGet handle token (imageIdToLink server)

hashPassword :: Password -> Password
hashPassword (Password p) = Password . pack . show . hashWith SHA256 $ encodeUtf8 p

getNewToken :: Monad m => Handle m -> Login -> Password -> m (Either Error Token)
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
