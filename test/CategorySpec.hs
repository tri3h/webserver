{-# LANGUAGE OverloadedStrings #-}

module CategorySpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import qualified Handlers.Category as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Category
  ( CategoryId (CategoryId),
    CreateCategory (..),
    GetCategory (..),
    Name (Name),
    categoryNotExist,
    invalidParent,
    noDeleteHasChildren,
    categoryNameTaken
  )

main :: IO ()
main = hspec $ do
  describe "Testing create getCategory" $ do
    it "Should successfully create" $ do
      let result = H.create handle createCategory
      result `shouldBe` return (Right ())
    it "Should fail if category name is already taken" $ do
      let handleCase =
            handle
              { H.hCreate = \_ -> return $ Left categoryNameTaken
              }
      let result = H.create handleCase createCategory
      result `shouldBe` return (Left categoryNameTaken)
    it "Should fail if parent doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left categoryNotExist)
              }
      let createCategoryCase =
            createCategory
              { cParentId = Just testParentId
              }
      let result = H.create handleCase createCategoryCase
      result `shouldBe` return (Left invalidParent)
  describe "Testing get getCategory" $ do
    it "Should successfully get" $ do
      let result = H.get handle testCategoryId
      result `shouldBe` return (Right getCategory)
    it "Should fail if getCategory doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left categoryNotExist)
              }
      let result = H.get handleCase testCategoryId
      result `shouldBe` return (Left categoryNotExist)
  describe "Testing delete getCategory" $ do
    it "Should successfullly delete" $ do
      let result = H.delete handle testCategoryId
      result `shouldBe` return (Right ())
    it "Should fail if getCategory doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left categoryNotExist)
              }
      let result = H.delete handleCase testCategoryId
      result `shouldBe` return (Left categoryNotExist)
    it "Should fail if getCategory has children" $ do
      let handleCase =
            handle
              { H.hGetChildren = \_ -> return [CategoryId 2]
              }
      let result = H.delete handleCase testCategoryId
      result `shouldBe` return (Left noDeleteHasChildren)
  describe "Testing edit getCategory" $ do
    it "Should successfully edit name" $ do
      let result = H.edit handle testCategoryId (Just $ Name "name") Nothing
      result `shouldBe` return (Right ())
    it "Should successfully edit parent id" $ do
      let result = H.edit handle testCategoryId Nothing (Just testParentId)
      result `shouldBe` return (Right ())
    it "Should fail if getCategory doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left categoryNotExist)
              }
      let result = H.edit handleCase testCategoryId Nothing (Just testParentId)
      result `shouldBe` return (Left categoryNotExist)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hCreate = \_ -> return $ Right (),
      H.hGet = \_ -> return getCategory,
      H.hGetAll = return [],
      H.hDelete = \_ -> return (),
      H.hEditName = \_ _ -> return (),
      H.hEditParent = \_ _ -> return (),
      H.hDoesExist = \_ -> return $ Right (),
      H.hGetChildren = \_ -> return []
    }

getCategory :: GetCategory
getCategory =
  GetCategory
    { gCategoryId = CategoryId 1,
      gName = Name "name",
      gParentId = Nothing
    }

createCategory :: CreateCategory
createCategory =
  CreateCategory
    { cName = Name "name",
      cParentId = Nothing
    }

testCategoryId :: CategoryId
testCategoryId = CategoryId 1

testParentId :: CategoryId
testParentId = CategoryId 2