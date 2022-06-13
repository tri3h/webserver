{-# LANGUAGE OverloadedStrings #-}

module Category where

import Data (categoryId, name, parentId, token)
import Data.Functor.Identity (Identity (Identity))
import Error (categoryNameTaken, categoryNotExist, invalidParent, noDeleteHasChildren, noSpecified)
import Handlers (categoryHandle, getCategory, loggerHandle, serverHandle, userHandle)
import qualified Handlers.Category as Category
import Handlers.Server (Handle (hCategory, hUser), makeNoTokenResponse)
import qualified Handlers.User as User
import Network.Wai (Request (pathInfo, queryString, requestMethod), defaultRequest)
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Category (GetCategory (GetCategory))
import Utility (response200JSON, response201, response204, response400, response404, showResp)

test :: IO ()
test = hspec $ do
  let req =
        defaultRequest
          { pathInfo = ["categories"]
          }
  describe "Testing create category" $ do
    let reqCreate =
          req
            { requestMethod = "POST",
              queryString =
                [ token,
                  name,
                  parentId
                ]
            }
    it "Should successs with parent id" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response201
      result `shouldBe` expectation
    it "Should successs without parent id" $ do
      let reqCreateCase = reqCreate {queryString = [token, name]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp $ response201
      result `shouldBe` expectation
    it "Should fail if user is not admin" $ do
      let serverHandleCase = serverHandle {hUser = userHandle {User.hIsAdmin = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqCreateCase = reqCreate {queryString = [name, parentId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no name" $ do
      let reqCreateCase = reqCreate {queryString = [token, parentId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "name"
      result `shouldBe` expectation
    it "Should fail if category already exists" $ do
      let serverHandleCase = serverHandle {hCategory = categoryHandle {Category.hCreate = \_ -> return $ Left categoryNameTaken}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response400 categoryNameTaken
      result `shouldBe` expectation
    it "Should fail if parent id is invalid" $ do
      let serverHandleCase = serverHandle {hCategory = categoryHandle {Category.hCreate = \_ -> return $ Left invalidParent}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response400 invalidParent
      result `shouldBe` expectation
  describe "Testing get category" $ do
    let reqGet =
          req
            { requestMethod = "GET",
              queryString =
                [ categoryId
                ]
            }
    it "Should get by category id" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response200JSON getCategory
      result `shouldBe` expectation
    it "Should get a list of categories" $ do
      let reqGetCase = reqGet {queryString = []}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetCase ""
      let expectation = Identity . showResp $ response200JSON ([] :: [GetCategory])
      result `shouldBe` expectation
    it "Should fail if category id is invalid" $ do
      let serverHandleCase = serverHandle {hCategory = categoryHandle {Category.hGet = \_ -> return $ Left categoryNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response400 categoryNotExist
      result `shouldBe` expectation
  describe "Testing edit category" $ do
    let reqEdit =
          req
            { requestMethod = "PUT",
              queryString =
                [ categoryId,
                  token,
                  parentId,
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
      let reqEditCase = reqEdit {queryString = [name, parentId, categoryId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEditCase ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no category id" $ do
      let reqEditCase = reqEdit {queryString = [name, parentId, token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEditCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "category_id"
      result `shouldBe` expectation
    it "Should fail if category id is invalid" $ do
      let serverHandleCase = serverHandle {hCategory = categoryHandle {Category.hEditName = \_ _ -> return $ Left categoryNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response400 categoryNotExist
      result `shouldBe` expectation
    it "Should fail if name is already taken" $ do
      let serverHandleCase = serverHandle {hCategory = categoryHandle {Category.hEditName = \_ _ -> return $ Left categoryNameTaken}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response400 categoryNameTaken
      result `shouldBe` expectation
    it "Should fail if parent id is invalid" $ do
      let serverHandleCase = serverHandle {hCategory = categoryHandle {Category.hEditParent = \_ _ -> return $ Left invalidParent}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response400 invalidParent
      result `shouldBe` expectation
  describe "Testing delete category" $ do
    let reqDelete =
          req
            { requestMethod = "DELETE",
              queryString =
                [ categoryId,
                  token
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
      let reqCase = reqDelete {queryString = [name, parentId, categoryId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCase ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if category id is invalid" $ do
      let serverHandleCase = serverHandle {hCategory = categoryHandle {Category.hDelete = \_ -> return $ Left categoryNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity . showResp $ response400 categoryNotExist
      result `shouldBe` expectation
    it "Should fail if category has children" $ do
      let serverHandleCase = serverHandle {hCategory = categoryHandle {Category.hDelete = \_ -> return $ Left noDeleteHasChildren}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity . showResp $ response400 noDeleteHasChildren
      result `shouldBe` expectation
