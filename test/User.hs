{-# LANGUAGE OverloadedStrings #-}

module User where

import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity (Identity))
import Error (invalidData, invalidToken, loginTaken, noImage, noSpecified, userNotExist)
import Handlers (getUser, loggerHandle, serverHandle, userHandle)
import Handlers.Server (makeNoTokenResponse)
import qualified Handlers.Server as Server
import qualified Handlers.User as User
import Network.HTTP.Types (QueryItem, status400)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Request (..), Response, defaultRequest, responseHeaders, responseStatus)
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.User (Password (Password))
import Utility (response200JSON, response201, response204, response400, response404, showResp)

main :: IO ()
main = hspec $ do
  describe "Testing create user" $ do
    let reqCreate =
          defaultRequest
            { pathInfo = ["users"],
              requestMethod = "POST",
              queryString =
                [ name,
                  surname,
                  login,
                  password
                ]
            }
    it "Should successfully create" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreate ""
      let expectation = Identity . showResp . response200JSON $ User.generateToken userHandle
      result `shouldBe` expectation
    it "Should fail if there is no name" $ do
      let reqCreateCase =
            reqCreate
              { queryString =
                  [ surname,
                    login,
                    password
                  ]
              }
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "name"
      result `shouldBe` expectation
    it "Should fail if there is no surname" $ do
      let reqCreateCase =
            reqCreate
              { queryString =
                  [ name,
                    login,
                    password
                  ]
              }
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "surname"
      result `shouldBe` expectation
    it "Should fail if there is no login" $ do
      let reqCreateCase =
            reqCreate
              { queryString =
                  [ surname,
                    name,
                    password
                  ]
              }
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "login"
      result `shouldBe` expectation
    it "Should fail if there is no password" $ do
      let reqCreateCase =
            reqCreate
              { queryString =
                  [ surname,
                    login,
                    name
                  ]
              }
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "password"
      result `shouldBe` expectation
    it "Should fail if login is already taken" $ do
      let serverHandleCase = serverHandle {Server.hUser = userHandle {User.hCreate = \_ -> return $ Left loginTaken}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response400 loginTaken
      result `shouldBe` expectation
    it "Should success with an avatar" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreate body
      let expectation = Identity . showResp . response200JSON $ User.generateToken userHandle
      result `shouldBe` expectation
  describe "Testing get user" $ do
    let reqGet =
          defaultRequest
            { pathInfo = ["users"],
              requestMethod = "GET",
              queryString =
                [ token
                ]
            }
    it "Should successfully get" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response200JSON getUser
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqGetCase = reqGet {queryString = []}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if token is invalid" $ do
      let serverHandleCase = serverHandle {Server.hUser = userHandle {User.hIsTokenValid = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response400 invalidToken
      result `shouldBe` expectation
  describe "Testing get new token" $ do
    let reqGetNew =
          defaultRequest
            { pathInfo = ["tokens"],
              queryString =
                [ login,
                  password
                ]
            }
    it "Should successfully get" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetNew ""
      let expectation = Identity . showResp . response200JSON $ User.generateToken userHandle
      result `shouldBe` expectation
    it "Should fail if there is no login" $ do
      let reqGetNewCase = reqGetNew {queryString = [password]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetNewCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "login"
      result `shouldBe` expectation
    it "Should fail if there is no password" $ do
      let reqGetNewCase = reqGetNew {queryString = [login]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetNewCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "password"
      result `shouldBe` expectation
    it "Should fail if login is invalid" $ do
      let serverHandleCase = serverHandle {Server.hUser = userHandle {User.hIsLoginValid = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqGetNew ""
      let expectation = Identity . showResp $ response400 invalidData
      result `shouldBe` expectation
    it "Should fail if password is invalid" $ do
      let serverHandleCase' = serverHandle {Server.hUser = userHandle {User.hFindPassword = \_ -> return $ Password ""}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase' loggerHandle "" reqGetNew ""
      let expectation = Identity . showResp $ response400 invalidData
      result `shouldBe` expectation
  describe "Testing add an avatar" $ do
    let reqAdd =
          defaultRequest
            { pathInfo = ["users", "avatar"],
              requestMethod = "POST",
              queryString = [token]
            }
    it "Should successfully add" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqAdd body
      let expectation = Identity . showResp $ response201
      result `shouldBe` expectation
    it "Should fail if there is no avatar" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqAdd ""
      let expectation = Identity . showResp $ response400 noImage
      result `shouldBe` expectation
    it "Should fail if token is invalid" $ do
      let serverHandleCase = serverHandle {Server.hUser = userHandle {User.hIsTokenValid = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqAdd body
      let expectation = Identity . showResp $ response400 invalidToken
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqAddCase = reqAdd {queryString = []}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqAddCase body
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
  describe "Testing delete user" $ do
    let reqDelete =
          defaultRequest
            { pathInfo = ["users"],
              requestMethod = "DELETE",
              queryString = [token, userId]
            }
    it "Should successfully delete" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDelete ""
      let expectation = Identity $ showResp response204
      result `shouldBe` expectation
    it "Should fail if user is not admin" $ do
      let serverHandleCase = serverHandle {Server.hUser = userHandle {User.hIsAdmin = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if token is invalid" $ do
      let serverHandleCase = serverHandle {Server.hUser = userHandle {User.hIsTokenValid = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity . showResp $ response400 invalidToken
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqDeleteCase = reqDelete {queryString = [userId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeleteCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if there is no user id" $ do
      let reqDeleteCase = reqDelete {queryString = [token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeleteCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "user_id"
      result `shouldBe` expectation
    it "Shoudl fail if user id is invalid" $ do
      let serverHandleCase = serverHandle {Server.hUser = userHandle {User.hDelete = \_ -> return $ Left userNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity . showResp $ response400 userNotExist
      result `shouldBe` expectation

name :: QueryItem
name = ("name", Just "name")

surname :: QueryItem
surname = ("surname", Just "surname")

login :: QueryItem
login = ("login", Just "login")

password :: QueryItem
password = ("password", Just "password")

token :: QueryItem
token = ("token", Just "token")

userId :: QueryItem
userId = ("user_id", Just "1")

body :: ByteString
body = "Content-Type: image/png \r name=\"avatar\" 12345 \r\n-"
