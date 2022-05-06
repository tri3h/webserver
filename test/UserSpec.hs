{-# LANGUAGE OverloadedStrings #-}

module UserSpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text, append, pack)
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
    invalidData,
    loginTaken,
    userNotExist,
  )

main :: IO ()
main = hspec $ do
  describe "Testing create fullUser" $ do
    it "Should successfully create" $ do
      let result = H.create handle createUser testAdmin
      result `shouldBe` (return $ Right testToken)
    it "Should fail if login isn't unique" $ do
      let handleCase =
            handle
              { H.hCreate = \_ -> return $ Left loginTaken
              }
      let result = H.create handleCase createUser testAdmin
      result `shouldBe` return (Left loginTaken)
  describe "Testing get fullUser" $
    it "Should successfully get" $ do
      let result = H.get handle serverAddress testToken
      result `shouldBe` return (Right getUser)
  describe "Testing delete fullUser" $ do
    it "Should successfully delete" $ do
      let result = H.delete handle testUserId
      result `shouldBe` return (Right ())
    it "Should fail if fullUser doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return $ Left userNotExist
              }
      let result = H.delete handleCase testUserId
      result `shouldBe` return (Left userNotExist)
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
      H.hDelete = \_ -> return (),
      H.hGetRandomNumber = return 1,
      H.hIsLoginValid = \_ -> return True,
      H.hFindPassword = \_ -> return $ Password "password",
      H.hUpdateToken = \_ _ -> return (),
      H.hDoesExist = \_ -> return $ Right ()
    }

fullUser :: FullUser
fullUser =
  FullUser
    { fName = Name "name",
      fSurname = Surname "surname",
      fAvatar = avatarImage,
      fLogin = Login "login",
      fPassword = Password "password",
      fAdmin = Admin False,
      fToken = Token "1234"
    }

getUser :: GetUser
getUser =
  GetUser
    { gUserId = UserId 1,
      gName = Name "name",
      gSurname = Surname "surname",
      gAvatar = avatarLink,
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
      cAvatar = avatarImage
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
