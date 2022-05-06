{-# LANGUAGE OverloadedStrings #-}

module AuthorSpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import qualified Handlers.Author as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Author
  ( Description (Description),
    AuthorId (AuthorId),
    CreateAuthor (..),
    EditAuthor (..),
    GetAuthor (..),
    authorNotExist,
  )
import Types.User (UserId (UserId), userNotExist)

main :: IO ()
main = hspec $ do
  describe "Testing get author" $ do
    it "Should successfully get" $ do
      let authorId = AuthorId 1
      let author =
            getAuthor
              { gAuthorId = authorId
              }
      let handleCase =
            handle
              { H.hGet = \_ -> return author
              }
      let result = H.get handle authorId
      result `shouldBe` return (Right author)
    it "Should fail if author doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return $ Left authorNotExist
              }
      let result = H.get handleCase testAuthorId
      result `shouldBe` return (Left authorNotExist)
  describe "Testing delete author" $ do
    it "Should successfully delete" $ do
      let result = H.delete handle testAuthorId
      result `shouldBe` return (Right ())
    it "Should fail if author doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return $ Left authorNotExist
              }
      let result = H.delete handleCase testAuthorId
      result `shouldBe` return (Left authorNotExist)
  describe "Testing edit author" $ do
    it "Should successfully edit" $ do
      let result = H.edit handle editAuthor
      result `shouldBe` return (Right ())
    it "Should fail if author doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return $ Left authorNotExist
              }
      let result = H.edit handleCase editAuthor
      result `shouldBe` return (Left authorNotExist)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hCreate = \_ -> return $ Right (),
      H.hGet = \_ -> return getAuthor,
      H.hDelete = \_ -> return (),
      H.hEdit = \_ -> return (),
      H.hDoesExist = \_ -> return $ Right ()
    }

editAuthor :: EditAuthor
editAuthor =
  EditAuthor
    { eAuthorId = AuthorId 1,
      eDescription = Description "edit"
    }

getAuthor :: GetAuthor
getAuthor =
  GetAuthor
    { gAuthorId = AuthorId 1,
      gUserId = Just $ UserId 1,
      gDescription = Description "get"
    }

testAuthorId :: AuthorId
testAuthorId = AuthorId 1