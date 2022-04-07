{-# LANGUAGE OverloadedStrings #-}
module UserSpec where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Data.Functor.Identity (Identity (Identity))
import qualified Handlers.User as H
import Types.User
    ( User(UserToCreate, User, UserToGet, admin, token, userId, date,
           name, surname, login, password, avatar),
      userNotExist,
      loginTaken,
      malformedUser,
      invalidData ) 
import Data.Text ( Text )
import Types.Image ( Image(Image, Link), malformedImage )

main :: IO ()
main = hspec $ do
    describe "Testing create user" $ do
        it "Should successfully create" $ do
            let result = H.create handle userToCreate
            let token = H.generateToken handle
            result `shouldBe` (Right <$> token)
        it "Should fail if login isn't unique" $ do
            let handleCase = handle {
                H.hIsLoginUnique = \_ -> return False
            }
            let result  = H.create handleCase userToCreate 
            result `shouldBe` return (Left loginTaken)
        it "Should fail if avatar format is incorrect" $ do
            let createUserCase = userToCreate {
                avatar = avatarLink
            }
            let result = H.create handle createUserCase
            result `shouldBe` return (Left malformedImage)
        it "Should fail if user format is incorrect (User)" $ do
            let result = H.create handle user 
            result `shouldBe` return (Left malformedUser)
        it "Should fail if user format is incorrect (UserToGet)" $ do
            let result = H.create handle userToGet 
            result `shouldBe` return (Left malformedUser)
    describe "Testing get user" $ do 
        it "Should successfully get" $ do
            let result = H.get handle "1234"
            result `shouldBe` return (Right userToGet)
        it "Should fail if avatar format is incorrect" $ do
            let userToGetCase = userToGet {
                avatar = avatarImage
            }
            let handleCase = handle {
                H.hGet = \_ -> return userToGetCase
            }
            let result = H.get handleCase "1234"
            result `shouldBe` return (Left malformedImage)
        it "Should fail if user format is incorrect (User)" $ do
            let handleCase = handle {
                H.hGet = \_ -> return user
            }
            let result = H.get handleCase "1234"
            result `shouldBe` return (Left malformedUser)
        it "Should fail if user format is incorrect (UserToCreate)" $ do
            let handleCase = handle {
                H.hGet = \_ -> return userToCreate
            }
            let result = H.get handleCase "1234"
            result `shouldBe` return (Left malformedUser)
    describe "Testing delete user" $ do 
        it "Should successfully delete" $ do
            let result = H.delete handle 1
            result `shouldBe` return (Right ())
        it "Should fail if user doesn't exist" $ do
            let handleCase = handle {
                H.hDoesExist = \_ -> return $ Left userNotExist
            }
            let result = H.delete handleCase 1
            result `shouldBe` return (Left userNotExist)
    describe "Testing get new token" $ do 
        it "Should successfully get" $ do
            let hashPass = H.hashPassword "password"
            let handleCase = handle {
                H.hFindPassword = \_ -> return hashPass
            }
            let result = H.getNewToken handleCase "login" "password"
            let token = H.generateToken handleCase
            result `shouldBe` (Right <$> token)
        it "Should fail if login isn't valid" $ do
            let handleCase = handle {
                H.hIsLoginValid = \_ -> return False
            }
            let result = H.getNewToken handleCase "login" "password"
            result `shouldBe` return (Left invalidData)
        it "Should fail if password isn't valid" $ do
            let handleCase = handle {
                H.hFindPassword = \_ -> return "1111"
            }
            let result = H.getNewToken handleCase "login" "2222"
            result `shouldBe` return (Left invalidData)

handle :: H.Handle Identity
handle = H.Handle {
    H.hIsLoginUnique = \_ -> return True,
    H.hIsTokenUnique = \_ -> return True,
    H.hCreate = \_ -> return (),
    H.hGet = \_ -> return userToGet,
    H.hDelete = \_ -> return (),
    H.hGetRandomNumber = return 1,
    H.hGetCurrentTime = return "01-01-2020",
    H.hIsLoginValid = \_ -> return True,
    H.hFindPassword = \_ -> return "password",
    H.hUpdateToken = \_ _ -> return (),
    H.hDoesExist = \_ -> return $ Right ()
}

user = User {
    name = "name",
    surname = "surname",
    avatar = avatarImage,
    login = "login",
    password = "password",
    date = "10-10-2020",
    admin = False,
    token = "1234"
}

userToGet = UserToGet {
    userId = 1,
    name = "name",
    surname = "surname",
    avatar = avatarLink,
    login = "login",
    date = "10-10-2020"
}

userToCreate = UserToCreate {
    name = "name",
    surname = "surname",
    login = "login",
    password = "password",
    avatar = avatarImage
}

avatarLink = Link "link"

avatarImage = Image "image" "imageType"