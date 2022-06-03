{-# LANGUAGE OverloadedStrings #-}

module Comment where

import Data.Functor.Identity (Identity (Identity))
import Error (commentNotExist, invalidToken, noPost, noSpecified, postNotExist)
import Handlers (commentHandle, loggerHandle, serverHandle, userHandle)
import qualified Handlers.Comment as Comment
import Handlers.Server (Handle (hComment, hUser), makeNoTokenResponse)
import qualified Handlers.User as User
import Network.HTTP.Types (QueryItem)
import Network.Wai (Request (pathInfo, queryString, requestMethod), defaultRequest)
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Comment (GetComment (GetComment))
import Utility (response200JSON, response201, response204, response400, response404, showResp)

main :: IO ()
main = hspec $ do
  describe "Testing get comments" $ do
    let reqGet =
          defaultRequest
            { pathInfo = ["comments"],
              requestMethod = "GET",
              queryString =
                [ postId
                ]
            }
    it "Should successfully get" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response200JSON ([] :: [GetComment])
      result `shouldBe` expectation
    it "Should fail if there is no post id" $ do
      let reqGetCase = reqGet {queryString = []}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqGetCase ""
      let expectation = Identity . showResp $ response400 $ noSpecified "post_id"
      result `shouldBe` expectation
    it "Should fail if there is no post" $ do
      let serverHandleCase = serverHandle {hComment = commentHandle {Comment.hGetEither = \_ _ _ -> return $ Left noPost}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqGet ""
      let expectation = Identity . showResp $ response400 noPost
      result `shouldBe` expectation
  describe "Testing create comment" $ do
    let reqCreate =
          defaultRequest
            { pathInfo = ["comments"],
              requestMethod = "POST",
              queryString =
                [ postId,
                  token,
                  text
                ]
            }
    it "Should successfully create" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreate ""
      let expectation = Identity $ showResp response201
      result `shouldBe` expectation
    it "Should fail if token is invalid" $ do
      let serverHandleCase = serverHandle {hUser = userHandle {User.hIsTokenValid = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response400 invalidToken
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqCreateCase = reqCreate {queryString = [postId, text]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no text" $ do
      let reqCreateCase = reqCreate {queryString = [token, postId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "text"
      result `shouldBe` expectation
    it "Should fail if there is no post id" $ do
      let reqCreateCase = reqCreate {queryString = [token, text]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCreateCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "post_id"
      result `shouldBe` expectation
    it "Should fail if post doesn't exist" $ do
      let serverHandleCase = serverHandle {hComment = commentHandle {Comment.hCreate = \_ -> return $ Left postNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCreate ""
      let expectation = Identity . showResp $ response400 postNotExist
      result `shouldBe` expectation
  describe "Testing delete comment" $ do
    let reqDelete =
          defaultRequest
            { pathInfo = ["comments"],
              requestMethod = "DELETE",
              queryString =
                [ commentId,
                  token
                ]
            }
    it "Should successfully delete" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDelete ""
      let expectation = Identity $ showResp response204
      result `shouldBe` expectation
    it "Should fail if user is not admin" $ do
      let serverHandleCase = serverHandle {hUser = userHandle {User.hIsAdmin = \_ -> return False}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity $ showResp response404
      result `shouldBe` expectation
    it "Should fail if there is no token" $ do
      let reqDeleteCase = reqDelete {queryString = [commentId]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeleteCase ""
      let expectation = Identity . showResp $ response404
      result `shouldBe` expectation
    it "Should fail if there is no comment id" $ do
      let reqDeleteCase = reqDelete {queryString = [token]}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqDeleteCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "comment_id"
      result `shouldBe` expectation
    it "Should fail if comment doesn't exist" $ do
      let serverHandleCase = serverHandle {hComment = commentHandle {Comment.hDelete = \_ -> return $ Left commentNotExist}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqDelete ""
      let expectation = Identity . showResp $ response400 commentNotExist
      result `shouldBe` expectation

postId :: QueryItem
postId = ("post_id", Just "1")

token :: QueryItem
token = ("token", Just "token")

text :: QueryItem
text = ("text", Just "text")

commentId :: QueryItem
commentId = ("comment_id", Just "1")
