{-# LANGUAGE OverloadedStrings #-}

module UserSpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text, append, pack)
import Error
  ( invalidData,
    loginTaken,
    userNotExist,
  )
import qualified Handlers.User as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Config (ServerAddress)
import Types.Image (Image (..), ImageId (ImageId), Link (Link), imageAddress)
import Types.User
  ( Admin (Admin),
    CreateUser (..),
    Date (Date),
    FullUser (..),
    GetUser (..),
    Login (Login),
    Name (Name),
    Password (Password),
    Surname (Surname),
    Token (Token),
    UserId (UserId),
  )

main :: IO ()
main = hspec $ do
  describe "Testing create user" $ do
    it "Should successfully create" $ do
      let result = H.create handle createUser testAdmin
      result `shouldBe` return (Right testToken)
    it "Should fail if login isn't unique" $ do
      let handleCase =
            handle
              { H.hCreate = \_ -> return $ Left loginTaken
              }
      let result = H.create handleCase createUser testAdmin
      result `shouldBe` return (Left loginTaken)
  describe "Testing get user" $
    it "Should successfully get" $ do
      let result = H.get handle serverAddress testToken
      result `shouldBe` return (Right getUser)
  describe "Testing get new token" $ do
    it "Should successfully get" $ do
      let hashPass = H.hashPassword testPassword
      let handleCase =
            handle
              { H.hFindPassword = \_ -> return hashPass
              }
      let result = H.getNewToken handleCase testLogin testPassword
      let token = H.generateToken handleCase
      result `shouldBe` (Right <$> token)
    it "Should fail if login isn't valid" $ do
      let handleCase =
            handle
              { H.hIsLoginValid = \_ -> return False
              }
      let result = H.getNewToken handleCase testLogin testPassword
      result `shouldBe` return (Left invalidData)
    it "Should fail if password isn't valid" $ do
      let handleCase =
            handle
              { H.hFindPassword = \_ -> return $ Password "1111"
              }
      let result = H.getNewToken handleCase testLogin testPassword
      result `shouldBe` return (Left invalidData)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hIsTokenUnique = \_ -> return True,
      H.hCreate = \_ -> return $ Right testToken,
      H.hGet = \_ _ -> return getUser,
      H.hDelete = \_ -> return $ Right (),
      H.hGetRandomNumber = return 1,
      H.hIsLoginValid = \_ -> return True,
      H.hFindPassword = \_ -> return $ Password "password",
      H.hUpdateToken = \_ _ -> return ()
    }

getUser :: GetUser
getUser =
  GetUser
    { gUserId = UserId 1,
      gName = Name "name",
      gSurname = Surname "surname",
      gAvatar = Just avatarLink,
      gLogin = Login "login",
      gDate = Date "10-10-2020"
    }

createUser :: CreateUser
createUser =
  CreateUser
    { cName = Name "name",
      cSurname = Surname "surname",
      cLogin = Login "login",
      cPassword = Password "password",
      cAvatar = Just avatarImage
    }

testToken :: Token
testToken = Token "123"

testLogin :: Login
testLogin = Login "login"

testPassword :: Password
testPassword = Password "Password"

testUserId :: UserId
testUserId = UserId 1

testAdmin :: Admin
testAdmin = Admin False

avatarLink :: Link
avatarLink = Link $ imageAddress `append` pack (show avatarId)

avatarImage :: Image
avatarImage = Image "image" "imageType"

avatarId :: ImageId
avatarId = ImageId 1

serverAddress :: ServerAddress
serverAddress = ""
