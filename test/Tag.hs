{-# LANGUAGE OverloadedStrings #-}

module Tag where

import Data.Functor.Identity (Identity (Identity))
import Error (invalidToken, noSpecified, tagNameTaken, tagNotExist)
import Handlers (loggerHandle, serverHandle, tag, tagHandle, userHandle)
import Handlers.Server (Handle (hTag, hUser), makeNoTokenResponse)
import qualified Handlers.Tag as Tag
import qualified Handlers.User as User
import Network.HTTP.Types (QueryItem)
import Network.Wai (Request (pathInfo, queryString, requestMethod), defaultRequest)
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Tag (Tag (Tag))
import Utility (response200JSON, response201, response204, response400, response404, showResp)

main :: IO ()
main = hspec $ do
  describe "Testing create tag" $ do
    let reqCreate =
          defaultRequest
            { pathInfo = ["tags"],
              requestMethod = "POST",
              queryString =
                [ token,
                  name
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
      let reqCreateCase = reqCreate {queryString = [name]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no name" $ do
      let reqCreateCase = reqCreate {queryString = [token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "name"
      result `shouldBe` expectation
    it "Should fail if tag already exists" $ do
      let serverHandleCase = serverHandle {hTag = tagHandle {Tag.hCreate = \_ -> return $ Left tagNameTaken}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response400 tagNameTaken
      result `shouldBe` expectation
  describe "Testing get tag" $ do
    let reqGet =
          defaultRequest
            { pathInfo = ["tags"],
              requestMethod = "GET",
              queryString =
                [ token,
                  tagId
                ]
            }
    it "Should successfully get by id" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response200JSON tag
      result `shouldBe` expectation
    it "Should successfully get a list of tags" $ do
      let reqGetCase = reqGet {queryString = [token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetCase ""
      let expectation = Identity . showResp $ response200JSON ([] :: [Tag])
      result `shouldBe` expectation
    it "Should fail if tag doesn't exist" $ do
      let serverHandleCase = serverHandle {hTag = tagHandle {Tag.hGet = \_ -> return $ Left tagNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response400 tagNotExist
      result `shouldBe` expectation
  describe "Testing edit tag" $ do
    let reqEdit =
          defaultRequest
            { pathInfo = ["tags"],
              requestMethod = "PUT",
              queryString =
                [ token,
                  tagId,
                  name
                ]
            }
    it "Should successfully edit" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response201
      result `shouldBe` expectation
    it "Should fail if user is not admin" $ do
      let serverHandleCase = serverHandle {hUser = userHandle {User.hIsAdmin = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqEditCase = reqEdit {queryString = [tagId, name]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEditCase ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no name" $ do
      let reqEditCase = reqEdit {queryString = [tagId, token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEditCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "name"
      result `shouldBe` expectation
    it "Should fail if there is no tag id" $ do
      let reqEditCase = reqEdit {queryString = [name, token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEditCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "tag_id"
      result `shouldBe` expectation
    it "Should fail if name already exists" $ do
      let serverHandleCase = serverHandle {hTag = tagHandle {Tag.hEdit = \_ -> return $ Left tagNameTaken}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response400 tagNameTaken
      result `shouldBe` expectation
    it "Should fail if tag doesn't exist" $ do
      let serverHandleCase = serverHandle {hTag = tagHandle {Tag.hEdit = \_ -> return $ Left tagNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response400 tagNotExist
      result `shouldBe` expectation
  describe "Testing delete tag" $ do
    let reqDelete =
          defaultRequest
            { pathInfo = ["tags"],
              requestMethod = "DELETE",
              queryString =
                [ token,
                  tagId
                ]
            }
    it "Should successfully delete" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDelete ""
      let expectation = Identity . showResp $ response204
      result `shouldBe` expectation
    it "Should fail if user is not admin" $ do
      let serverHandleCase = serverHandle {hUser = userHandle {User.hIsAdmin = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqDeleteCase = reqDelete {queryString = [tagId, name]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeleteCase ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if tag doesn't exist" $ do
      let serverHandleCase = serverHandle {hTag = tagHandle {Tag.hDelete = \_ -> return $ Left tagNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity . showResp $ response400 tagNotExist
      result `shouldBe` expectation
    it "Should fail if there is no tag id" $ do
      let reqDeleteCase = reqDelete {queryString = [name, token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeleteCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "tag_id"
      result `shouldBe` expectation

token :: QueryItem
token = ("token", Just "token")

name :: QueryItem
name = ("name", Just "name")

tagId :: QueryItem
tagId = ("tag_id", Just "1")
