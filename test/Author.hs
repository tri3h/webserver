{-# LANGUAGE OverloadedStrings #-}

module Author where

import Data.Functor.Identity (Identity (Identity))
import Error (alreadyAuthor, authorNotExist, invalidToken, noSpecified)
import Handlers (authorHandle, getAuthor, loggerHandle, serverHandle, userHandle)
import qualified Handlers.Author as Author
import Handlers.Server (Handle (hAuthor, hUser), makeNoTokenResponse)
import qualified Handlers.User as User
import Network.HTTP.Types (QueryItem)
import Network.Wai (Request (pathInfo, queryString, requestMethod), defaultRequest)
import Test.Hspec (describe, hspec, it, shouldBe)
import Utility (response200JSON, response201, response204, response400, response404, showResp)

main :: IO ()
main = hspec $ do
  describe "Testing create author" $ do
    let reqCreate =
          defaultRequest
            { pathInfo = ["authors"],
              requestMethod = "POST",
              queryString =
                [ token,
                  userId,
                  description
                ]
            }
    it "Should successfully create" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response201
      result `shouldBe` expectation
    it "Should fail if user is not admin" $ do
      let serverHandleCase = serverHandle {hUser = userHandle {User.hIsAdmin = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqCreateCase = reqCreate {queryString = [userId, description]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if there is no user id" $ do
      let reqCreateCase = reqCreate {queryString = [token, description]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "user_id"
      result `shouldBe` expectation
    it "Should fail if there is no description" $ do
      let reqCreateCase = reqCreate {queryString = [token, userId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "description"
      result `shouldBe` expectation
    it "Should fail if user is already author" $ do
      let serverHandleCase = serverHandle {hAuthor = authorHandle {Author.hCreate = \_ -> return $ Left alreadyAuthor}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response400 alreadyAuthor
      result `shouldBe` expectation
  describe "Testing get author" $ do
    let reqGet =
          defaultRequest
            { pathInfo = ["authors"],
              requestMethod = "GET",
              queryString =
                [ token,
                  authorId
                ]
            }
    it "Should successfully get" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response200JSON getAuthor
      result `shouldBe` expectation
    it "Should fail if user is not admin" $ do
      let serverHandleCase = serverHandle {hUser = userHandle {User.hIsAdmin = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqGetCase = reqGet {queryString = [authorId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if there is no author id" $ do
      let reqGetCase = reqGet {queryString = [token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "author_id"
      result `shouldBe` expectation
    it "Should fail if author doesn't exist" $ do
      let serverHandleCase = serverHandle {hAuthor = authorHandle {Author.hGet = \_ -> return $ Left authorNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response400 authorNotExist
      result `shouldBe` expectation
  describe "Testing edit author" $ do
    let reqEdit =
          defaultRequest
            { pathInfo = ["authors"],
              requestMethod = "PUT",
              queryString =
                [ token,
                  authorId,
                  description
                ]
            }
    it "Should successfully edit" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEdit ""
      let expectation = Identity $ showResp response201
      result `shouldBe` expectation
    it "Should fail if user is not admin" $ do
      let serverHandleCase = serverHandle {hUser = userHandle {User.hIsAdmin = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqEditCase = reqEdit {queryString = [authorId, description]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEditCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if there is no author id" $ do
      let reqEditCase = reqEdit {queryString = [token, description]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEditCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "author_id"
      result `shouldBe` expectation
    it "Should fail if there is no description" $ do
      let reqEditCase = reqEdit {queryString = [authorId, token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEditCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "description"
      result `shouldBe` expectation
    it "Should fail if author doesn't exist" $ do
      let serverHandleCase = serverHandle {hAuthor = authorHandle {Author.hEdit = \_ -> return $ Left authorNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response400 authorNotExist
      result `shouldBe` expectation
  describe "Testing delete author" $ do
    let reqDelete =
          defaultRequest
            { pathInfo = ["authors"],
              requestMethod = "DELETE",
              queryString =
                [ token,
                  authorId
                ]
            }
    it "Should successfully delete" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDelete ""
      let expectation = Identity $ showResp response204
      result `shouldBe` expectation
    it "Should fail if user is not admin" $ do
      let serverHandleCase = serverHandle {hUser = userHandle {User.hIsAdmin = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqDeleteCase = reqDelete {queryString = [authorId, description]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeleteCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if there is no author id" $ do
      let reqDeleteCase = reqDelete {queryString = [token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeleteCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "author_id"
      result `shouldBe` expectation
    it "Should fail if author doesn't exist" $ do
      let serverHandleCase = serverHandle {hAuthor = authorHandle {Author.hEdit = \_ -> return $ Left authorNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity $ showResp response204
      result `shouldBe` expectation

token :: QueryItem
token = ("token", Just "token")

name :: QueryItem
name = ("name", Just "name")

description :: QueryItem
description = ("description", Just "description")

userId :: QueryItem
userId = ("user_id", Just "1")

authorId :: QueryItem
authorId = ("author_id", Just "1")
