{-# LANGUAGE OverloadedStrings #-}

module CategorySpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import qualified Handlers.Category as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Category (Category (..), categoryNotExist, invalidParent, malformedCategory, noDeleteHasChildren)

main :: IO ()
main = hspec $ do
  describe "Testing create category" $ do
    it "Should successfully create" $ do
      let result = H.create handle categoryToCreate
      result `shouldBe` return (Right ())
    it "Should fail if category format isn't correct" $ do
      let result = H.create handle category
      result `shouldBe` return (Left malformedCategory)
    it "Should fail if parent doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left categoryNotExist)
              }
      let categoryToCreateCase =
            categoryToCreate
              { parentId = Just 2
              }
      let result = H.create handleCase categoryToCreateCase
      result `shouldBe` return (Left invalidParent)
  describe "Testing get category" $ do
    it "Should successfully get" $ do
      let result = H.get handle 1
      result `shouldBe` return (Right category)
    it "Should fail if category doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left categoryNotExist)
              }
      let result = H.get handleCase 1
      result `shouldBe` return (Left categoryNotExist)
    it "Should fail if category format isn't correct" $ do
      let handleCase =
            handle
              { H.hGet = \_ -> return categoryToCreate
              }
      let result = H.get handleCase 1
      result `shouldBe` return (Left malformedCategory)
  describe "Testing delete category" $ do
    it "Should successfullly delete" $ do
      let result = H.delete handle 1
      result `shouldBe` return (Right ())
    it "Should fail if category doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left categoryNotExist)
              }
      let result = H.delete handleCase 1
      result `shouldBe` return (Left categoryNotExist)
    it "Should fail if category has children" $ do
      let handleCase =
            handle
              { H.hGetChildren = \_ -> return [2]
              }
      let result = H.delete handleCase 1
      result `shouldBe` return (Left noDeleteHasChildren)
  describe "Testing edit category" $ do
    it "Should successfully edit name" $ do
      let result = H.edit handle 1 (Just "name") Nothing
      result `shouldBe` return (Right ())
    it "Should successfully edit parent id" $ do
      let result = H.edit handle 1 Nothing (Just 2)
      result `shouldBe` return (Right ())
    it "Should fail if category doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left categoryNotExist)
              }
      let result = H.edit handleCase 1 Nothing (Just 2)
      result `shouldBe` return (Left categoryNotExist)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hCreate = \_ -> return (),
      H.hGet = \_ -> return category,
      H.hDelete = \_ -> return (),
      H.hEditName = \_ _ -> return (),
      H.hEditParent = \_ _ -> return (),
      H.hDoesExist = \_ -> return $ Right (),
      H.hGetChildren = \_ -> return []
    }

category :: Category
category =
  Category
    { categoryId = 1,
      name = "name",
      parentId = Nothing
    }

categoryToCreate :: Category
categoryToCreate =
  CategoryToCreate
    { name = "name",
      parentId = Nothing
    }
