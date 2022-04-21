{-# LANGUAGE OverloadedStrings #-}

module DraftSpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text, append, pack)
import qualified Handlers.Draft as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Author (authorNotExist)
import Types.Category (categoryNotExist)
import Types.Config (ServerAddress)
import Types.Draft (Draft (..), EditParams (..), draftNotExist, noDeleteHasPost, noDraftAuthor, userNotAuthor)
import Types.Image (Image (..), malformedImage, imageAddress)
import Types.Tag (tagNotExist)

main :: IO ()
main = hspec $ do
  describe "Testing create draft" $ do
    it "Should successfully create" $ do
      let result = H.create handle draft
      result `shouldBe` return (Right ())
    it "Should fail if author doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesAuthorExist = \_ -> return (Left authorNotExist)
              }
      let result = H.create handleCase draft
      result `shouldBe` return (Left authorNotExist)
    it "Should fail if category doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesCategoryExist = \_ -> return (Left categoryNotExist)
              }
      let result = H.create handleCase draft
      result `shouldBe` return (Left categoryNotExist)
    it "Should fail if tag doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesTagExist = \_ -> return (Left tagNotExist)
              }
      let result = H.create handleCase draft
      result `shouldBe` return (Left tagNotExist)
    it "Should fail if main photo format is incorrect" $ do
      let draftCase =
            draft
              { mainPhoto = link
              }
      let result = H.create handle draftCase
      result `shouldBe` return (Left malformedImage)
  describe "Testing get draft" $ do
    it "Should successfully get" $ do
      let draftCase =
            draft
              { mainPhoto = link
              }
      let handleCase =
            handle
              { H.hGet = \_ -> return draftCase
              }
      let result = H.get handleCase serverAddress 1 "1234"
      result `shouldBe` return (Right draftCase)
    it "Should fail if main photo format is incorrect" $ do
      let result = H.get handle serverAddress 1 "1234"
      result `shouldBe` return (Left malformedImage)
    it "Should fail if minor photo format is incorrect" $ do
      let draftCase =
            draft
              { minorPhoto = [image]
              }
      let handleCase =
            handle
              { H.hGet = \_ -> return draftCase
              }
      let result = H.get handleCase serverAddress 1 "1234"
      result `shouldBe` return (Left malformedImage)
    it "Should fail if draft doesn't exist" $ do
      let draftCase =
            draft
              { mainPhoto = link
              }
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left draftNotExist),
                H.hGet = \_ -> return draftCase
              }
      let result = H.get handleCase serverAddress 1 "1234"
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of draft" $ do
      let draftCase =
            draft
              { mainPhoto = link
              }
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return 2,
                H.hGet = \_ -> return draftCase
              }
      let result = H.get handleCase serverAddress 1 "1234"
      result `shouldBe` return (Left noDraftAuthor)
    it "Shoudl fail if user is not author at all" $ do
      let draftCase =
            draft
              { mainPhoto = link
              }
      let handleCase =
            handle
              { H.hIsAuthor = \_ -> return False,
                H.hGet = \_ -> return draftCase
              }
      let result = H.get handleCase serverAddress 1 "1234"
      result `shouldBe` return (Left userNotAuthor)
  describe "Testing edit draft" $ do
    it "Should successfully edit" $ do
      let result = H.edit handle 1 "1234" editParams
      result `shouldBe` return (Right ())
    it "Should edit main photo" $ do
      let editParamsCase =
            editParams
              { eMainPhoto = Just image
              }
      let result = H.edit handle 1 "1234" editParamsCase
      result `shouldBe` return (Right ())
    it "Should edit category id" $ do
      let editParamsCase =
            editParams
              { eCategoryId = Just 1
              }
      let result = H.edit handle 1 "1234" editParamsCase
      result `shouldBe` return (Right ())
    it "Should edit tag id" $ do
      let editParamsCase =
            editParams
              { eTagId = Just [1]
              }
      let result = H.edit handle 1 "1234" editParamsCase
      result `shouldBe` return (Right ())
    it "Should edit name" $ do
      let editParamsCase =
            editParams
              { eName = Just "name"
              }
      let result = H.edit handle 1 "1234" editParamsCase
      result `shouldBe` return (Right ())
    it "Should edit description" $ do
      let editParamsCase =
            editParams
              { eDescription = Just "d"
              }
      let result = H.edit handle 1 "1234" editParamsCase
      result `shouldBe` return (Right ())
    it "Should fail if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left draftNotExist)
              }
      let result = H.edit handleCase 1 "1234" editParams
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of the draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return 2
              }
      let result = H.edit handleCase 1 "1234" editParams
      result `shouldBe` return (Left noDraftAuthor)
    it "Should fail if main photo format is incorrect" $ do
      let editParamsCase =
            editParams
              { eMainPhoto = Just link
              }
      let result = H.edit handle 1 "1234" editParamsCase
      result `shouldBe` return (Left malformedImage)
    it "Should fail if category doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesCategoryExist = \_ -> return (Left categoryNotExist)
              }
      let editParamsCase =
            editParams
              { eCategoryId = Just 2
              }
      let result = H.edit handleCase 1 "1234" editParamsCase
      result `shouldBe` return (Left categoryNotExist)
    it "Should fail if tag doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesTagExist = \_ -> return (Left tagNotExist)
              }
      let editParamsCase =
            editParams
              { eTagId = Just [1]
              }
      let result = H.edit handleCase 1 "1234" editParamsCase
      result `shouldBe` return (Left tagNotExist)
  describe "Testing delete draft" $ do
    it "Should successfully delete" $ do
      let handleCase =
            handle
              { H.hHasPost = \_ -> return False
              }
      let result = H.delete handleCase 1 "1234"
      result `shouldBe` return (Right ())
    it "Should fail if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left draftNotExist),
                H.hHasPost = \_ -> return False
              }
      let result = H.delete handleCase 1 "1234"
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return 2,
                H.hHasPost = \_ -> return False
              }
      let result = H.delete handleCase 1 "1234"
      result `shouldBe` return (Left noDraftAuthor)
    it "Should fail if draft has post" $ do
      let result = H.delete handle 1 "1234"
      result `shouldBe` return (Left noDeleteHasPost)
  describe "Testing publish draft" $ do
    it "Should successfully publish if has post" $ do
      let result = H.publish handle 1 "1234"
      result `shouldBe` return (Right ())
    it "Should successfully publish if has no post" $ do
      let handleCase =
            handle
              { H.hHasPost = \_ -> return False
              }
      let result = H.publish handleCase 1 "1234"
      result `shouldBe` return (Right ())
    it "Should fail if draft doesn't exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left draftNotExist)
              }
      let result = H.publish handleCase 1 "1234"
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author at all" $ do
      let handleCase =
            handle
              { H.hIsAuthor = \_ -> return False
              }
      let result = H.publish handleCase 1 "1234"
      result `shouldBe` return (Left userNotAuthor)
    it "Should fail if user is not author of draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return 2
              }
      let result = H.publish handleCase 1 "1234"
      result `shouldBe` return (Left noDraftAuthor)
  describe "Testing add minor photos" $ do
    it "Should add minor photo" $ do
      let result = H.addMinorPhoto handle 1 "1234" image
      result `shouldBe` return (Right ())
    it "Should fail if minor photo format is incorrect" $ do
      let result = H.addMinorPhoto handle 1 "1234" link
      result `shouldBe` return (Left malformedImage)
    it "Should fail if draft doesn'exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left draftNotExist)
              }
      let result = H.addMinorPhoto handleCase 1 "1234" image
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of the draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return 2
              }
      let result = H.addMinorPhoto handleCase 1 "1234" image
      result `shouldBe` return (Left noDraftAuthor)
  describe "Testing delete minor photos" $ do
    it "Should delete minor photo" $ do
      let result = H.deleteMinorPhoto handle 1 "1234" 1
      result `shouldBe` return (Right ())
    it "Should fail if draft doesn'exist" $ do
      let handleCase =
            handle
              { H.hDoesExist = \_ -> return (Left draftNotExist)
              }
      let result = H.deleteMinorPhoto handleCase 1 "1234" 1
      result `shouldBe` return (Left draftNotExist)
    it "Should fail if user is not author of the draft" $ do
      let handleCase =
            handle
              { H.hGetAuthorIdByToken = \_ -> return 2
              }
      let result = H.deleteMinorPhoto handleCase 1 "1234" 1
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
      H.hGetCurrentDate = return "01-01-2020",
      H.hGet = \_ -> return draft,
      H.hGetAuthor = \_ -> return 1,
      H.hPublish = \_ _ -> return (),
      H.hUpdate = \_ -> return (),
      H.hGetAuthorIdByToken = \_ -> return 1,
      H.hDoesExist = \_ -> return (Right ()),
      H.hDoesAuthorExist = \_ -> return (Right ()),
      H.hDoesCategoryExist = \_ -> return (Right ()),
      H.hDoesTagExist = \_ -> return (Right ())
    }

draft :: Draft
draft =
  Draft
    { draftId = Nothing,
      postId = Nothing,
      authorId = 1,
      categoryId = 1,
      tagId = [1],
      name = "name",
      description = "description",
      mainPhoto = image,
      minorPhoto = []
    }

editParams :: EditParams
editParams =
  EditParams
    { eCategoryId = Nothing,
      eTagId = Nothing,
      eName = Nothing,
      eDescription = Nothing,
      eMainPhoto = Nothing
    }

image :: Image
image = Image "image" "imageType"

link :: Image
link = Link $ imageAddress `append` pack (show imageId)

imageId :: Image
imageId = Id 1

serverAddress :: ServerAddress
serverAddress = ""
