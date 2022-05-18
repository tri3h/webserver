{-# LANGUAGE OverloadedStrings #-}

module CategorySpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import Error (categoryNotExist, invalidParent, noDeleteHasChildren)
import qualified Handlers.Category as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Category
  ( CategoryId (CategoryId),
    CreateCategory (..),
    GetCategory (..),
    Name (Name),
  )

main :: IO ()
main = hspec $ do
  describe "Testing delete category" $ do
    it "Should successfullly delete" $ do
      let result = H.delete handle testCategoryId
      result `shouldBe` return (Right ())
    it "Should fail if category doesn't exist" $ do
      let handleCase =
            handle
              { H.hDelete = \_ -> return (Left categoryNotExist)
              }
      let result = H.delete handleCase testCategoryId
      result `shouldBe` return (Left categoryNotExist)
    it "Should fail if category has children" $ do
      let handleCase =
            handle
              { H.hGetChildren = \_ -> return [CategoryId 2]
              }
      let result = H.delete handleCase testCategoryId
      result `shouldBe` return (Left noDeleteHasChildren)
  describe "Testing edit category" $ do
    it "Should successfully only edit name" $ do
      let result = H.edit handle testCategoryId (Just testName) Nothing
      result `shouldBe` return (Right ())
    it "Should successfully edit only parent id" $ do
      let result = H.edit handle testCategoryId Nothing (Just testParentId)
      result `shouldBe` return (Right ())
    it "Should successfully edit name and parent id" $ do
      let result = H.edit handle testCategoryId (Just testName) (Just testParentId)
      result `shouldBe` return (Right ())
    it "Should fail edit name if category doesn't exist" $ do
      let handleCase =
            handle
              { H.hEditName = \_ _ -> return (Left categoryNotExist)
              }
      let result = H.edit handleCase testCategoryId (Just testName) Nothing
      result `shouldBe` return (Left categoryNotExist)
    it "Should fail edit parent id if category doesn't exist" $ do
      let handleCase =
            handle
              { H.hEditParent = \_ _ -> return (Left categoryNotExist)
              }
      let result = H.edit handleCase testCategoryId Nothing (Just testParentId)
      result `shouldBe` return (Left categoryNotExist)
    it "Should fail edit parent id if new parent is child of category" $ do
      let handleCase =
            handle
              { H.hGetChildren = \x -> return [x]
              }
      let result = H.edit handleCase testCategoryId Nothing (Just testCategoryId)
      result `shouldBe` return (Left invalidParent)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hDelete = \_ -> return $ Right (),
      H.hEditName = \_ _ -> return $ Right (),
      H.hEditParent = \_ _ -> return $ Right (),
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

testName :: Name
testName = Name "Name"
