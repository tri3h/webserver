{-# LANGUAGE OverloadedStrings #-}

module DraftSpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text, append, pack)
import Error
  ( authorNotExist,
    categoryNotExist,
    draftNotExist,
    noDeleteHasPost,
    noDraftAuthor,
    tagNotExist,
    userNotAuthor,
  )
import qualified Handlers.Draft as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Author (AuthorId (AuthorId))
import Types.Category (CategoryId (CategoryId))
import Types.Config (ServerAddress)
import Types.Draft
  ( CreateDraft (..),
    DraftId (DraftId),
    EditParams (..),
    GetDraft (..),
    Name (Name),
  )
import Types.Image (Image (..), ImageId (ImageId), Link (Link), imageAddress)
import Types.Tag (TagId (TagId))
import Types.User (Token (Token))

main :: IO ()
main = hspec $ do
  describe "Testing get draft" $ do
    it "Should successfully get" $ do
      let result = H.get handle serverAddress testDraftId testToken
      result `shouldBe` return (Right getDraft)
    it "Should fail if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByDraftId = \_ -> return (Left draftNotExist)
              }
      let result = H.get handleCase serverAddress testDraftId testToken
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return $ Right authorId
              }
      let result = H.get handleCase serverAddress testDraftId testToken
      result `shouldBe` return (Left noDraftAuthor)
    it "Shoudl fail if user is not author at all" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return (Left userNotAuthor)
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
              { eName = Just $ Name "name"
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
    it "Should fail edit name if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hEditName = \_ _ -> return (Left draftNotExist)
              }
      let editParamsCase =
            editParams
              { eName = editName
              }
      let result = H.edit handleCase testDraftId testToken editParamsCase
      result `shouldBe` return (Left draftNotExist)
    it "Should fail edit description if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hEditDescription = \_ _ -> return (Left draftNotExist)
              }
      let editParamsCase =
            editParams
              { eText = editText
              }
      let result = H.edit handleCase testDraftId testToken editParamsCase
      result `shouldBe` return (Left draftNotExist)
    it "Should fail edit category id if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hEditCategoryId = \_ _ -> return (Left draftNotExist)
              }
      let editParamsCase =
            editParams
              { eCategoryId = editCategoryId
              }
      let result = H.edit handleCase testDraftId testToken editParamsCase
      result `shouldBe` return (Left draftNotExist)
    it "Should fail edit tag id if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hEditTagId = \_ _ -> return (Left draftNotExist)
              }
      let editParamsCase =
            editParams
              { eTagId = editTagId
              }
      let result = H.edit handleCase testDraftId testToken editParamsCase
      result `shouldBe` return (Left draftNotExist)
    it "Should fail edit main photo if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hEditMainPhoto = \_ _ -> return (Left draftNotExist)
              }
      let editParamsCase =
            editParams
              { eMainPhoto = editMainPhoto
              }
      let result = H.edit handleCase testDraftId testToken editParamsCase
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of the draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return . Right $ AuthorId 2
              }
      let result = H.edit handleCase testDraftId testToken editParams
      result `shouldBe` return (Left noDraftAuthor)
    it "Should fail if category doesn't exist" $ do
      let handleCase =
            handle
              { H.hEditCategoryId = \_ _ -> return (Left categoryNotExist)
              }
      let editParamsCase =
            editParams
              { eCategoryId = editCategoryId
              }
      let result = H.edit handleCase testDraftId testToken editParamsCase
      result `shouldBe` return (Left categoryNotExist)
    it "Should fail if tag doesn't exist" $ do
      let handleCase =
            handle
              { H.hEditTagId = \_ _ -> return (Left tagNotExist)
              }
      let editParamsCase =
            editParams
              { eTagId = editTagId
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
              { H.hDelete = \_ -> return (Left draftNotExist),
                H.hHasPost = \_ -> return False
              }
      let result = H.delete handleCase testDraftId testToken
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return $ Right authorId,
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
              { H.hPublish = \_ -> return (Left draftNotExist),
                H.hHasPost = \_ -> return False
              }
      let result = H.publish handleCase testDraftId testToken
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author at all" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return (Left userNotAuthor)
              }
      let result = H.publish handleCase testDraftId testToken
      result `shouldBe` return (Left userNotAuthor)
    it "Should fail if user is not author of draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return $ Right authorId
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
              { H.hAddMinorPhoto = \_ _ -> return (Left draftNotExist)
              }
      let result = H.addMinorPhoto handleCase testDraftId testToken image
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of the draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return $ Right authorId
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
              { H.hDeleteMinorPhoto = \_ _ -> return (Left draftNotExist)
              }
      let result = H.deleteMinorPhoto handleCase testDraftId testToken imageId
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of the draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return $ Right authorId
              }
      let result = H.deleteMinorPhoto handleCase testDraftId testToken imageId
      result `shouldBe` return (Left noDraftAuthor)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hCreate = \_ -> return $ Right (),
      H.hEditCategoryId = \_ _ -> return $ Right (),
      H.hEditTagId = \_ _ -> return $ Right (),
      H.hEditName = \_ _ -> return $ Right (),
      H.hEditDescription = \_ _ -> return $ Right (),
      H.hEditMainPhoto = \_ _ -> return $ Right (),
      H.hDelete = \_ -> return $ Right (),
      H.hAddMinorPhoto = \_ _ -> return $ Right (),
      H.hDeleteMinorPhoto = \_ _ -> return $ Right (),
      H.hHasPost = \_ -> return True,
      H.hGet = \_ _ -> return getDraft,
      H.hGetAuthorIdByDraftId = \_ -> return . Right $ AuthorId 1,
      H.hPublish = \_ -> return $ Right (),
      H.hUpdate = \_ -> return $ Right (),
      H.hGetAuthorIdByToken = \_ -> return . Right $ AuthorId 1
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
    { cToken = Token "123",
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

editCategoryId :: Maybe CategoryId
editCategoryId = Just $ CategoryId 2

editTagId :: Maybe [TagId]
editTagId = Just [TagId 2]

editName :: Maybe Name
editName = Just $ Name "name"

editText :: Maybe Text
editText = Just "text"

editMainPhoto :: Maybe Image
editMainPhoto = Just image

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
