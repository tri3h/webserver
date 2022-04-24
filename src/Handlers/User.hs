{-# LANGUAGE RecordWildCards #-}

module Handlers.User (Login, Token, UserId, Handle (..), create, get, delete, getNewToken, generateToken, hashPassword) where

import Crypto.Hash (SHA256 (SHA256), hashWith)
import qualified Data.ByteString.Char8 as Char8
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Types.Config (ServerAddress)
import Types.Image (Image (Image), malformedImage)
import Types.User
  ( Admin (Admin),
    CreateUser (..),
    Date (Date),
    FullUser (..),
    GetUser (gAvatar),
    Login,
    Password (Password),
    Token (Token),
    UserId,
    invalidData,
    loginTaken,
  )
import Utility (imageToLink)

data Handle m = Handle
  { hIsLoginUnique :: Login -> m Bool,
    hIsTokenUnique :: Token -> m Bool,
    hCreate :: FullUser -> m (),
    hGet :: Token -> m GetUser,
    hDelete :: UserId -> m (),
    hGetRandomNumber :: m Integer,
    hGetCurrentTime :: m String,
    hIsLoginValid :: Login -> m Bool,
    hFindPassword :: Login -> m Password,
    hUpdateToken :: Login -> Token -> m (),
    hDoesExist :: UserId -> m (Either Text ())
  }

create :: Monad m => Handle m -> CreateUser -> m (Either Text Token)
create handle partUser = do
  isUnique <- hIsLoginUnique handle $ cLogin partUser
  if isUnique
    then case cAvatar partUser of
      Image _ _ -> do
        fToken <- generateToken handle
        time <- hGetCurrentTime handle
        let fDate = Date . pack $ take 10 time
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
      _ -> return $ Left malformedImage
    else return $ Left loginTaken

get :: Monad m => Handle m -> ServerAddress -> Token -> m (Either Text GetUser)
get handle server token = do
  user <- hGet handle token
  let maybeLink = imageToLink server $ gAvatar user
  case maybeLink of
    Right link -> return $ Right user {gAvatar = link}
    Left l -> return $ Left l

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
