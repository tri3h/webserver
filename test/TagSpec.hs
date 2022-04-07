{-# LANGUAGE OverloadedStrings #-}
module TagSpec where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Data.Functor.Identity (Identity (Identity))
import qualified Handler.Tag as H
import Types.Tag ( Tag(Tag, name, tagId), tagNotExist )
import Data.Text ( Text )

main :: IO ()
main = hspec $ do
    describe "Testing create tag" $ do
        it "Should successfully create" $ do
            let result = H.create handle "name"
            result `shouldBe` return (Right ())
    describe "Testing get tag" $ do
        it "Should successfully get" $ do
            let result = H.get handle 1
            result `shouldBe` return (Right tag)
        it "Should fail if tag doesn't exist" $ do
            let handleCase = handle {
                H.hDoesExist = \_ -> return (Left tagNotExist)
            }
            let result = H.get handleCase 1
            result `shouldBe` return (Left tagNotExist)
    describe "Testing edit tag" $ do
        it "Should successfully edit" $ do
            let result = H.edit handle tag
            result `shouldBe` return (Right ())
        it "Should fail if tag doesn't exist" $ do
            let handleCase = handle {
                H.hDoesExist = \_ -> return (Left tagNotExist)
            }
            let result = H.edit handleCase tag
            result `shouldBe` return (Left tagNotExist)
    describe "Testing delete tag" $ do
        it "Should successfully delete" $ do
            let result = H.delete handle 1
            result `shouldBe` return (Right ())
        it "Should fail if tag doesn't exist" $ do
            let handleCase = handle {
                H.hDoesExist = \_ -> return (Left tagNotExist)
            }
            let result = H.delete handleCase 1
            result `shouldBe` return (Left tagNotExist)

handle :: H.Handle Identity
handle = H.Handle {
    H.hCreate = \_ -> return (),
    H.hGet = \_ -> return tag,
    H.hDelete = \_ -> return (),
    H.hEdit = \_ -> return (),
    H.hDoesExist = \_ -> return (Right ())
}

tag = Tag {
    name = "name",
    tagId = 1
}