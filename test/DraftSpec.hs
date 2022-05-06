{-# LANGUAGE OverloadedStrings #-}

module DraftSpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text, append, pack)
import qualified Handlers.Draft as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Author (authorNotExist, AuthorId (AuthorId))
import Types.Category (categoryNotExist, CategoryId (CategoryId))
import Types.Config (ServerAddress)
import Types.Draft
  ( CreateDraft (..),
    EditParams (..),
    GetDraft (..),
    draftNotExist,
    noDeleteHasPost,
    noDraftAuthor,
    userNotAuthor, DraftId (DraftId), Name (Name)
  )
import Types.Image (Image (..), imageAddress, ImageId (ImageId), Link (Link))
import Types.Tag (tagNotExist, TagId (TagId))
import Types.User (Token(Token))

main :: IO ()
main = hspec $ do
  describe "Testing create draft" $ do
    it "Should successfully create" $ do
      let result = H.create handle createDraft
      result `shouldBe` return (Right ())
    it "Should fail if author doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesAuthorExist = \_ -> return (Left authorNotExist)
              }
      let result = H.create handleCase createDraft
      result `shouldBe` return (Left authorNotExist)
    it "Should fail if category doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesCategoryExist = \_ -> return (Left categoryNotExist)
              }
      let result = H.create handleCase createDraft
      result `shouldBe` return (Left categoryNotExist)
    it "Should fail if tag doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesTagExist = \_ -> return (Left tagNotExist)
              }
      let result = H.create handleCase createDraft
      result `shouldBe` return (Left tagNotExist)
  describe "Testing get draft" $ do
    it "Should successfully get" $ do
      let result = H.get handle serverAddress testDraftId testToken
      result `shouldBe` return (Right getDraft)
    it "Should fail if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left draftNotExist)
              }
      let result = H.get handleCase serverAddress testDraftId testToken
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return authorId
              }
      let result = H.get handleCase serverAddress testDraftId testToken
      result `shouldBe` return (Left noDraftAuthor)
    it "Shoudl fail if user is not author at all" $ do
      let handleCase =
            handle
              { H.hIsAuthor = \_ -> return False
              }
      let result = H.get handleCase serverAddress testDraftId testToken
      result `shouldBe` return (Left userNotAuthor)
  describe "Testing edit draft" $ do
    it "Should successfully edit" $ do
      let result = H.edit handle testDraftId testToken editParams
      result `shouldBe` return (Right ())
    it "Should edit main photo" $ do
      let editParamsCase =
            editParams
              { eMainPhoto = Just image
              }
      let result = H.edit handle testDraftId testToken editParamsCase
      result `shouldBe` return (Right ())
    it "Should edit category id" $ do
      let editParamsCase =
            editParams
              { eCategoryId = Just categoryId
              }
      let result = H.edit handle testDraftId testToken editParamsCase
      result `shouldBe` return (Right ())
    it "Should edit tag id" $ do
      let editParamsCase =
            editParams
              { eTagId = Just [TagId 1]
              }
      let result = H.edit handle testDraftId testToken editParamsCase
      result `shouldBe` return (Right ())
    it "Should edit name" $ do
      let editParamsCase =
            editParams
              { eName = Just $ Name"name"
              }
      let result = H.edit handle testDraftId testToken editParamsCase
      result `shouldBe` return (Right ())
    it "Should edit description" $ do
      let editParamsCase =
            editParams
              { eText = Just "d"
              }
      let result = H.edit handle testDraftId testToken editParamsCase
      result `shouldBe` return (Right ())
    it "Should fail if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left draftNotExist)
              }
      let result = H.edit handleCase testDraftId testToken editParams
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of the draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return $ AuthorId 2
              }
      let result = H.edit handleCase testDraftId testToken editParams
      result `shouldBe` return (Left noDraftAuthor)
    it "Should fail if category doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesCategoryExist = \_ -> return (Left categoryNotExist)
              }
      let editParamsCase =
            editParams
              { eCategoryId = Just categoryId
              }
      let result = H.edit handleCase testDraftId testToken editParamsCase
      result `shouldBe` return (Left categoryNotExist)
    it "Should fail if tag doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesTagExist = \_ -> return (Left tagNotExist)
              }
      let editParamsCase =
            editParams
              { eTagId = Just [TagId 1]
              }
      let result = H.edit handleCase testDraftId testToken editParamsCase
      result `shouldBe` return (Left tagNotExist)
  describe "Testing delete draft" $ do
    it "Should successfully delete" $ do
      let handleCase =
            handle
              { H.hHasPost = \_ -> return False
              }
      let result = H.delete handleCase testDraftId testToken
      result `shouldBe` return (Right ())
    it "Should fail if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left draftNotExist),
                H.hHasPost = \_ -> return False
              }
      let result = H.delete handleCase testDraftId testToken
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return authorId,
                H.hHasPost = \_ -> return False
              }
      let result = H.delete handleCase testDraftId testToken
      result `shouldBe` return (Left noDraftAuthor)
    it "Should fail if draft has post" $ do
      let result = H.delete handle testDraftId testToken
      result `shouldBe` return (Left noDeleteHasPost)
  describe "Testing publish draft" $ do
    it "Should successfully publish if has post" $ do
      let result = H.publish handle testDraftId testToken
      result `shouldBe` return (Right ())
    it "Should successfully publish if has no post" $ do
      let handleCase =
            handle
              { H.hHasPost = \_ -> return False
              }
      let result = H.publish handleCase testDraftId testToken
      result `shouldBe` return (Right ())
    it "Should fail if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left draftNotExist)
              }
      let result = H.publish handleCase testDraftId testToken
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author at all" $ do
      let handleCase =
            handle
              { H.hIsAuthor = \_ -> return False
              }
      let result = H.publish handleCase testDraftId testToken
      result `shouldBe` return (Left userNotAuthor)
    it "Should fail if user is not author of draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return authorId
              }
      let result = H.publish handleCase testDraftId testToken
      result `shouldBe` return (Left noDraftAuthor)
  describe "Testing add minor photos" $ do
    it "Should add minor photo" $ do
      let result = H.addMinorPhoto handle testDraftId testToken image
      result `shouldBe` return (Right ())
    it "Should fail if draft doesn'exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left draftNotExist)
              }
      let result = H.addMinorPhoto handleCase testDraftId testToken image
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of the draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return authorId
              }
      let result = H.addMinorPhoto handleCase testDraftId testToken image
      result `shouldBe` return (Left noDraftAuthor)
  describe "Testing delete minor photos" $ do
    it "Should delete minor photo" $ do
      let result = H.deleteMinorPhoto handle testDraftId testToken imageId
      result `shouldBe` return (Right ())
    it "Should fail if draft doesn'exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left draftNotExist)
              }
      let result = H.deleteMinorPhoto handleCase testDraftId testToken imageId
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of the draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return authorId
              }
      let result = H.deleteMinorPhoto handleCase testDraftId testToken imageId
      result `shouldBe` return (Left noDraftAuthor)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hCreate = \_ -> return (),
      H.hEditCategoryId = \_ _ -> return (),
      H.hEditTagId = \_ _ -> return (),
      H.hEditName = \_ _ -> return (),
      H.hEditDescription = \_ _ -> return (),
      H.hEditMainPhoto = \_ _ -> return (),
      H.hDelete = \_ -> return (),
      H.hAddMinorPhoto = \_ _ -> return (),
      H.hDeleteMinorPhoto = \_ _ -> return (),
      H.hIsAuthor = \_ -> return True,
      H.hHasPost = \_ -> return True,
      H.hGet = \_ _ -> return getDraft,
      H.hGetAuthor = \_ -> return $ AuthorId 1,
      H.hPublish = \_ -> return (),
      H.hUpdate = \_ -> return (),
      H.hGetAuthorIdByToken = \_ -> return $ AuthorId 1,
      H.hDoesExist = \_ -> return (Right ()),
      H.hDoesAuthorExist = \_ -> return (Right ()),
      H.hDoesCategoryExist = \_ -> return (Right ()),
      H.hDoesTagExist = \_ -> return (Right ())
    }

getDraft :: GetDraft
getDraft =
  GetDraft
    { gDraftId = DraftId 1,
      gPostId = Nothing,
      gAuthorId = Just $ AuthorId 1,
      gCategoryId = Just $ CategoryId 1,
      gTagId = [TagId 1],
      gName = Name "name",
      gText = "description",
      gMainPhoto = link,
      gMinorPhoto = []
    }

createDraft :: CreateDraft
createDraft =
  CreateDraft
    { 
      cAuthorId = AuthorId 1,
      cCategoryId = CategoryId 1,
      cTagId = [TagId 1],
      cName = Name "name",
      cText = "description",
      cMainPhoto = image
    }

editParams :: EditParams
editParams =
  EditParams
    { eCategoryId = Nothing,
      eTagId = Nothing,
      eName = Nothing,
      eText = Nothing,
      eMainPhoto = Nothing
    }

testDraftId :: DraftId
testDraftId = DraftId 1

testToken :: Token
testToken = Token "123"

image :: Image
image = Image "image" "imageType"

link :: Link
link = Link $ imageAddress `append` pack (show imageId)

imageId :: ImageId
imageId = ImageId 1

authorId :: AuthorId
authorId = AuthorId 2

categoryId :: CategoryId
categoryId = CategoryId 1

serverAddress :: ServerAddress
serverAddress = ""
