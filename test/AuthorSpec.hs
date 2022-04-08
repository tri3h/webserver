{-# LANGUAGE OverloadedStrings #-}

module AuthorSpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import qualified Handlers.Author as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Author (Author (..), authorNotExist, malformedAuthor)
import Types.User (userNotExist)

main :: IO ()
main = hspec $ do
  describe "Testing create author" $ do
    it "Should successfully create" $ do
      let result = H.create handle authorToCreate
      result `shouldBe` return (Right ())
    it "Should fail if author format is incorrect (AuthorToEdit)" $ do
      let result = H.create handle authorToEdit
      result `shouldBe` return (Left malformedAuthor)
    it "Should fail if author format is incorrect (AuthorToGet)" $ do
      let result = H.create handle authorToGet
      result `shouldBe` return (Left malformedAuthor)
    it "Should fail if user doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesUserExist = \_ -> return $ Left userNotExist
              }
      let result = H.create handleCase authorToCreate
      result `shouldBe` return (Left userNotExist)
  describe "Testing get author" $ do
    it "Should successfully get" $ do
      let authorId = 1
      let author =
            authorToGet
              { authorId = authorId
              }
      let handleCase =
            handle
              { H.hGet = \aId -> return author
              }
      let result = H.get handle authorId
      result `shouldBe` return (Right author)
    it "Should fail if author doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return $ Left authorNotExist
              }
      let result = H.get handleCase 1
      result `shouldBe` return (Left authorNotExist)
    it "Should fail if author format is incorrect (AuthorToEdit)" $ do
      let handleCase =
            handle
              { H.hGet = \_ -> return authorToEdit
              }
      let result = H.get handleCase 1
      result `shouldBe` return (Left malformedAuthor)
    it "Should fail if author format is incorrect (AuthorToCreate)" $ do
      let handleCase =
            handle
              { H.hGet = \_ -> return authorToCreate
              }
      let result = H.get handleCase 1
      result `shouldBe` return (Left malformedAuthor)
  describe "Testing delete author" $ do
    it "Should successfully delete" $ do
      let result = H.delete handle 1
      result `shouldBe` return (Right ())
    it "Should fail if author doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return $ Left authorNotExist
              }
      let result = H.delete handleCase 1
      result `shouldBe` return (Left authorNotExist)
  describe "Testing edit author" $ do
    it "Should successfully edit" $ do
      let result = H.edit handle authorToEdit
      result `shouldBe` return (Right ())
    it "Should fail if author format is incorrect (AuthorToCreate)" $ do
      let result = H.edit handle authorToCreate
      result `shouldBe` return (Left malformedAuthor)
    it "Should fail if author format is incorrect (AuthorToGet)" $ do
      let result = H.edit handle authorToGet
      result `shouldBe` return (Left malformedAuthor)
    it "Should fail if author doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return $ Left authorNotExist
              }
      let result = H.edit handleCase authorToEdit
      result `shouldBe` return (Left authorNotExist)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hCreate = \_ -> return (),
      H.hGet = \_ -> return authorToGet,
      H.hDelete = \_ -> return (),
      H.hEdit = \_ -> return (),
      H.hDoesExist = \_ -> return $ Right (),
      H.hDoesUserExist = \_ -> return $ Right ()
    }

authorToEdit :: Author
authorToEdit =
  AuthorToEdit
    { authorId = 1,
      description = "edit"
    }

authorToCreate :: Author
authorToCreate =
  AuthorToCreate
    { userId = 1,
      description = "create"
    }

authorToGet :: Author
authorToGet =
  AuthorToGet
    { authorId = 1,
      userId = 1,
      description = "get"
    }
