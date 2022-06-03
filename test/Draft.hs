{-# LANGUAGE OverloadedStrings #-}

module Draft where

import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity (Identity))
import Database.Queries.Draft (hasPost)
import Error (categoryNotExist, draftNotExist, imageNotExist, invalidToken, noDeleteHasPost, noDraftAuthor, noImage, noSpecified, tagNotExist, userNotAuthor)
import Handlers (draftHandle, getDraft, loggerHandle, serverHandle)
import qualified Handlers.Draft as Draft
import Handlers.Server (Handle (hDraft), makeNoTokenResponse)
import Network.HTTP.Types (QueryItem)
import Network.Wai (Request (pathInfo, queryString, requestMethod), defaultRequest)
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Author (AuthorId (AuthorId))
import Types.Draft (DraftId (DraftId), GetDraft (gDraftId))
import Utility (response200JSON, response201, response204, response400, response404, showResp)

main :: IO ()
main = hspec $ do
  describe "Testing get draft" $ do
    let reqGet =
          defaultRequest
            { pathInfo = ["drafts"],
              requestMethod = "GET",
              queryString =
                [ token,
                  draftId
                ]
            }
    it "Should successfully get" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response200JSON getDraft
      result `shouldBe` expectation
    it "Should fail if user is not author of the draft" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hGetAuthorIdByDraftId = \_ -> return . Right $ AuthorId 2}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response400 noDraftAuthor
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqGetCase = reqGet {queryString = [draftId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if draft id is invalid" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hGetAuthorIdByDraftId = \_ -> return $ Left draftNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response400 draftNotExist
      result `shouldBe` expectation
    it "Should fail if there is no draft id" $ do
      let reqGetCase = reqGet {queryString = [token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "draft_id"
      result `shouldBe` expectation
  describe "Testing get list of draft id" $ do
    let reqGetList =
          defaultRequest
            { pathInfo = ["drafts", "id"],
              requestMethod = "GET",
              queryString =
                [ token
                ]
            }
    it "Should successfully get" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetList ""
      let expectation = Identity . showResp $ response200JSON ([] :: [DraftId])
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqGetListCase = reqGetList {queryString = []}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetListCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
  describe "Testing publish draft" $ do
    let reqPublish =
          defaultRequest
            { pathInfo = ["publish"],
              queryString =
                [ token,
                  draftId
                ]
            }
    it "Should success if there is no post" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hHasPost = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqPublish ""
      let expectation = Identity $ showResp response201
      result `shouldBe` expectation
    it "Should success if there is post" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqPublish ""
      let expectation = Identity $ showResp response201
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqPublishCase = reqPublish {queryString = []}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqPublishCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if user is not author of the draft" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hGetAuthorIdByDraftId = \_ -> return . Right $ AuthorId 2}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqPublish ""
      let expectation = Identity . showResp $ response400 noDraftAuthor
      result `shouldBe` expectation
    it "Should fail if there is no draft id" $ do
      let reqPublishCase = reqPublish {queryString = [token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqPublishCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "draft_id"
      result `shouldBe` expectation
    it "Should fail if draft doesn't exist" $ do
      let serverHandleCase =
            serverHandle
              { hDraft =
                  draftHandle
                    { Draft.hPublish = \_ -> return $ Left draftNotExist,
                      Draft.hHasPost = \_ -> return False
                    }
              }
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqPublish ""
      let expectation = Identity . showResp $ response400 draftNotExist
      result `shouldBe` expectation
  describe "Testing create draft" $ do
    let reqCreate =
          defaultRequest
            { pathInfo = ["drafts"],
              requestMethod = "POST",
              queryString =
                [ token,
                  categoryId,
                  name,
                  description,
                  tagId
                ]
            }
    it "Should success with tags" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response200JSON (gDraftId getDraft)
      result `shouldBe` expectation
    it "Should success without tags" $ do
      let reqCreateCase = reqCreate {queryString = [token, categoryId, name, description]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp $ response200JSON (gDraftId getDraft)
      result `shouldBe` expectation
    it "Should success with main photo" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreate mainPhoto
      let expectation = Identity . showResp $ response200JSON (gDraftId getDraft)
      result `shouldBe` expectation
    it "Should fail if user is not author" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hCreate = \_ -> return $ Left userNotAuthor}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response400 userNotAuthor
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqCreateTokenCase = reqCreate {queryString = [categoryId, name, description, tagId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateTokenCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if there is no category id" $ do
      let reqCreateCategoryCase = reqCreate {queryString = [token, name, description, tagId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCategoryCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "category_id"
      result `shouldBe` expectation
    it "Should fail if there is no description" $ do
      let reqCreateDescrCase = reqCreate {queryString = [categoryId, name, token, tagId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateDescrCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "description"
      result `shouldBe` expectation
    it "Should fail if there is no name" $ do
      let reqCreateNameCase = reqCreate {queryString = [categoryId, token, description, tagId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateNameCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "name"
      result `shouldBe` expectation
    it "Should fail if category id is invalid" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hCreate = \_ -> return $ Left categoryNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response400 categoryNotExist
      result `shouldBe` expectation
    it "Should fail if tag id are invalid" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hCreate = \_ -> return $ Left tagNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response400 tagNotExist
      result `shouldBe` expectation
  describe "Testing add minor photo to a draft" $ do
    let reqAdd =
          defaultRequest
            { pathInfo = ["drafts", "minor_photo"],
              requestMethod = "POST",
              queryString =
                [ token,
                  draftId
                ]
            }
    it "Should successfully add" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqAdd minorPhoto
      let expectation = Identity . showResp $ response201
      result `shouldBe` expectation
    it "Should fail if user is not author of the draft" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hGetAuthorIdByDraftId = \_ -> return . Right $ AuthorId 2}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqAdd minorPhoto
      let expectation = Identity . showResp $ response400 noDraftAuthor
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqAddCase = reqAdd {queryString = [draftId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqAddCase minorPhoto
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if there is no draft id" $ do
      let reqAddCase = reqAdd {queryString = [token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqAddCase minorPhoto
      let expectation = Identity . showResp . response400 $ noSpecified "draft_id"
      result `shouldBe` expectation
    it "Should fail if there is no image" $ do
      let reqAddCase = reqAdd {queryString = [draftId, token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqAddCase ""
      let expectation = Identity . showResp $ response400 noImage
      result `shouldBe` expectation
    it "Should fail if draft doesn't exist" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hAddMinorPhoto = \_ _ -> return $ Left draftNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqAdd minorPhoto
      let expectation = Identity . showResp $ response400 draftNotExist
      result `shouldBe` expectation
  describe "Testing edit draft" $ do
    let reqEdit =
          defaultRequest
            { pathInfo = ["drafts"],
              requestMethod = "PUT",
              queryString =
                [ token,
                  draftId,
                  categoryId,
                  tagId,
                  name,
                  description
                ]
            }
    it "Should success with multiple parameters" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response201
      result `shouldBe` expectation
    it "Should success with one parameter" $ do
      let reqEditCase = reqEdit {queryString = [token, draftId, name]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEditCase ""
      let expectation = Identity . showResp $ response201
      result `shouldBe` expectation
    it "Should success with main photo" $ do
      let reqEditCase = reqEdit {queryString = [token, draftId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEditCase mainPhoto
      let expectation = Identity . showResp $ response201
      result `shouldBe` expectation
    it "Should fail if user is not author of the draft" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hGetAuthorIdByDraftId = \_ -> return . Right $ AuthorId 2}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response400 noDraftAuthor
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqEditCase = reqEdit {queryString = [draftId, name]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEditCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if there is no draft id" $ do
      let reqEditCase = reqEdit {queryString = [token, name]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqEditCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "draft_id"
      result `shouldBe` expectation
    it "Should fail if draft doesn't exist" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hEditName = \_ _ -> return $ Left draftNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response400 draftNotExist
      result `shouldBe` expectation
    it "Should fail if category id is invalid" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hEditCategoryId = \_ _ -> return $ Left categoryNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response400 categoryNotExist
      result `shouldBe` expectation
    it "Should fail if tag id are invalid" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hEditTagId = \_ _ -> return $ Left tagNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqEdit ""
      let expectation = Identity . showResp $ response400 tagNotExist
      result `shouldBe` expectation
  describe "Testing delete draft" $ do
    let reqDelete =
          defaultRequest
            { pathInfo = ["drafts"],
              requestMethod = "DELETE",
              queryString =
                [ token,
                  draftId
                ]
            }
    it "Should success if there is no post" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hHasPost = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity $ showResp response204
      result `shouldBe` expectation
    it "Should fail if there is post" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDelete ""
      let expectation = Identity . showResp $ response400 noDeleteHasPost
      result `shouldBe` expectation
    it "Should fail if user is not author of the draft" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hGetAuthorIdByDraftId = \_ -> return . Right $ AuthorId 2}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity . showResp $ response400 noDraftAuthor
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqDeleteCase = reqDelete {queryString = [draftId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeleteCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if there is no draft id" $ do
      let reqDeleteCase = reqDelete {queryString = [token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeleteCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "draft_id"
      result `shouldBe` expectation
    it "Should fail if draft doesn't exist" $ do
      let serverHandleCase =
            serverHandle
              { hDraft =
                  draftHandle
                    { Draft.hDelete = \_ -> return $ Left draftNotExist,
                      Draft.hHasPost = \_ -> return False
                    }
              }
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity . showResp $ response400 draftNotExist
      result `shouldBe` expectation
  describe "Testing delete minor photo from a draft" $ do
    let reqDeletePhoto =
          defaultRequest
            { pathInfo = ["drafts", "minor_photo"],
              requestMethod = "DELETE",
              queryString =
                [ token,
                  draftId,
                  minorPhotoId
                ]
            }
    it "Should successfully delete" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeletePhoto ""
      let expectation = Identity $ showResp response204
      result `shouldBe` expectation
    it "Should fail if user is not author of the draft" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hGetAuthorIdByDraftId = \_ -> return . Right $ AuthorId 2}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDeletePhoto ""
      let expectation = Identity . showResp $ response400 noDraftAuthor
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqDeletePhotoCase = reqDeletePhoto {queryString = [draftId, minorPhotoId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeletePhotoCase ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if there is no draft id" $ do
      let reqDeletePhotoCase = reqDeletePhoto {queryString = [token, minorPhotoId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeletePhotoCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "draft_id"
      result `shouldBe` expectation
    it "Should fail if there is no minor photo id" $ do
      let reqDeletePhotoCase = reqDeletePhoto {queryString = [token, draftId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeletePhotoCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "minor_photo_id"
      result `shouldBe` expectation
    it "Should fail if draft doesn't exist" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hDeleteMinorPhoto = \_ _ -> return $ Left draftNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDeletePhoto ""
      let expectation = Identity . showResp $ response400 draftNotExist
      result `shouldBe` expectation
    it "Should fail if minor photo doesn't exist" $ do
      let serverHandleCase = serverHandle {hDraft = draftHandle {Draft.hDeleteMinorPhoto = \_ _ -> return $ Left imageNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDeletePhoto ""
      let expectation = Identity . showResp $ response400 imageNotExist
      result `shouldBe` expectation

token :: QueryItem
token = ("token", Just "token")

draftId :: QueryItem
draftId = ("draft_id", Just "1")

categoryId :: QueryItem
categoryId = ("category_id", Just "1")

description :: QueryItem
description = ("description", Just "description")

name :: QueryItem
name = ("name", Just "name")

tagId :: QueryItem
tagId = ("tag_id", Just "1")

minorPhotoId :: QueryItem
minorPhotoId = ("minor_photo_id", Just "1")

mainPhoto :: ByteString
mainPhoto = "Content-Type: image/png \r name=\"main_photo\" 12345 \r\n-"

minorPhoto :: ByteString
minorPhoto = "Content-Type: image/png \r name=\"minor_photo\" 12345 \r\n-"
