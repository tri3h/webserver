{-# LANGUAGE OverloadedStrings #-}

module TagSpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import qualified Handlers.Tag as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Tag (Tag (Tag, name, tagId), tagNotExist, TagId (TagId), Name (Name))

main :: IO ()
main = hspec $ do
  describe "Testing create tag" $
    it "Should successfully create" $ do
      let result = H.create handle $ Name "name"
      result `shouldBe` return (Right ())
  describe "Testing get tag" $ do
    it "Should successfully get" $ do
      let result = H.get handle testTagId
      result `shouldBe` return (Right tag)
    it "Should fail if tag doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left tagNotExist)
              }
      let result = H.get handleCase testTagId
      result `shouldBe` return (Left tagNotExist)
  describe "Testing edit tag" $ do
    it "Should successfully edit" $ do
      let result = H.edit handle tag
      result `shouldBe` return (Right ())
    it "Should fail if tag doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left tagNotExist)
              }
      let result = H.edit handleCase tag
      result `shouldBe` return (Left tagNotExist)
  describe "Testing delete tag" $ do
    it "Should successfully delete" $ do
      let result = H.delete handle testTagId
      result `shouldBe` return (Right ())
    it "Should fail if tag doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left tagNotExist)
              }
      let result = H.delete handleCase testTagId
      result `shouldBe` return (Left tagNotExist)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hCreate = \_ -> return (),
      H.hGet = \_ -> return tag,
      H.hDelete = \_ -> return (),
      H.hEdit = \_ -> return (),
      H.hDoesExist = \_ -> return (Right ())
    }

tag :: Tag
tag =
  Tag
    { name = Name "name",
      tagId = testTagId
    }

testTagId :: TagId
testTagId = TagId 1
