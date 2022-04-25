{-# LANGUAGE OverloadedStrings #-}

module CommentSpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import qualified Handlers.Comment as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Comment
  ( CommentId (CommentId),
    CreateComment (..),
    GetComment (..),
    commentNotExist,
  )
import Types.Post (postNotExist)
import Types.PostComment (PostId (PostId))
import Types.User (UserId (UserId))

main :: IO ()
main = hspec $ do
  describe "Testing create comment" $ do
    it "Should successfully create" $ do
      let result = H.create handle createComment
      result `shouldBe` return (Right ())
    it "Should fail if post doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesPostExist = \_ -> return (Left postNotExist)
              }
      let result = H.create handleCase createComment
      result `shouldBe` return (Left postNotExist)
  describe "Testing get comment" $ do
    it "Should successfully get" $ do
      let result = H.get handle testPostId
      result `shouldBe` return (Right [getComment])
    it "Should fail if post doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesPostExist = \_ -> return (Left postNotExist)
              }
      let result = H.get handleCase testPostId
      result `shouldBe` return (Left postNotExist)
  describe "Testing delete comment" $ do
    it "Should successfully delete" $ do
      let result = H.delete handle testCommentId
      result `shouldBe` return (Right ())
    it "Should fail if comment doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left commentNotExist)
              }
      let result = H.delete handleCase testCommentId
      result `shouldBe` return (Left commentNotExist)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hGet = \_ -> return [getComment],
      H.hCreate = \_ -> return (),
      H.hDelete = \_ -> return (),
      H.hDoesExist = \_ -> return (Right ()),
      H.hDoesPostExist = \_ -> return (Right ())
    }

createComment :: CreateComment
createComment =
  CreateComment
    { cPostId = PostId 1,
      cUserId = UserId 1,
      cText = "text"
    }

getComment :: GetComment
getComment =
  GetComment
    { gCommentId = CommentId 1,
      gUserId = Just $ UserId 1,
      gText = "text"
    }

testPostId :: PostId
testPostId = PostId 1

testCommentId :: CommentId
testCommentId = CommentId 1