{-# LANGUAGE OverloadedStrings #-}
module CommentSpec where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Data.Functor.Identity (Identity (Identity))
import qualified Handler.Comment as H
import Types.Comment ( Comment(..), malformedComment, commentNotExist )
import Types.Post ( postNotExist )
import Data.Text ( Text )

main :: IO ()
main = hspec $ do
    describe "Testing create comment" $ do
        it "Should successfully create" $ do
            let result = H.create handle commentToCreate 
            result `shouldBe` return (Right ())
        it "Should fail if comment format is incorrect" $ do
            let result = H.create handle commentToGet
            result `shouldBe` return (Left malformedComment)
        it "Should fail if post doesn't exist" $ do
            let handleCase = handle {
                H.hDoesPostExist = \_ -> return (Left postNotExist)
            }
            let result = H.create handleCase commentToCreate
            result `shouldBe` return (Left postNotExist)
    describe "Testing get comment" $ do
        it "Should successfully get" $ do
            let result = H.get handle 1
            result `shouldBe` return (Right [commentToGet])
        it "Should fail if comment format is incorrect" $ do
            let handleCase = handle {
                H.hGet = \_ -> return [commentToCreate]
            }
            let result = H.get handleCase 1
            result `shouldBe` return (Left malformedComment)
        it "Should fail if post doesn't exist" $ do
            let handleCase = handle {
                H.hDoesPostExist = \_ -> return (Left postNotExist)
            }
            let result = H.get handleCase 1
            result `shouldBe` return (Left postNotExist)
    describe "Testing delete comment" $ do
        it "Should successfully delete" $ do
            let result = H.delete handle 1
            result `shouldBe` return (Right ())
        it "Should fail if comment doesn't exist" $ do
            let handleCase = handle {
                H.hDoesExist = \_ -> return (Left commentNotExist)
            }
            let result = H.delete handleCase 1
            result `shouldBe` return (Left commentNotExist)

handle :: H.Handle Identity
handle = H.Handle {
    H.hGet = \_ -> return [commentToGet],
    H.hCreate = \_ -> return (),
    H.hDelete = \_ -> return (),
    H.hDoesExist = \_ -> return (Right ()),
    H.hDoesPostExist = \_ -> return (Right ())
}

commentToCreate :: Comment
commentToCreate = CommentToCreate {
    postId = 1,
    userId = 1,
    text = "text"
}

commentToGet :: Comment
commentToGet = CommentToGet {
    commentId = 1,
    userId = 1,
    text = "text"
}