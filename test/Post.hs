{-# LANGUAGE OverloadedStrings #-}

module Post where

import Data.Functor.Identity (Identity (Identity))
import Error (noPost)
import Handlers (fullPost, loggerHandle, postHandle, serverHandle)
import qualified Handlers.Post as Post
import Handlers.Server (Handle (hPost), makeNoTokenResponse)
import Network.HTTP.Types (QueryItem)
import Network.Wai (Request (pathInfo, queryString, requestMethod), defaultRequest)
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Post (FullPost)
import Utility (response200JSON, response400, showResp)

main :: IO ()
main = hspec $
  describe "Testing get post" $ do
    let req =
          defaultRequest
            { pathInfo = ["posts"],
              requestMethod = "GET"
            }
    it "Should success without parameters" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" req ""
      let expectation = Identity . showResp $ response200JSON [fullPost]
      result `shouldBe` expectation
    it "Should success with multiple parameters" $ do
      let reqCase = req {queryString = [authorId, postName, tag]}
      let result' = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCase ""
      let expectation = Identity . showResp $ response200JSON [fullPost]
      result' `shouldBe` expectation
    it "Should success with all parameters" $ do
      let reqCase =
            req
              { queryString =
                  [ authorId,
                    categoryId,
                    tag,
                    tagId,
                    tagIn,
                    tagAll,
                    postName,
                    text,
                    substring,
                    dateAfter,
                    dateBefore,
                    sortBy
                  ]
              }
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCase ""
      let expectation = Identity . showResp $ response200JSON [fullPost]
      result `shouldBe` expectation
    it "Should fail if there are no posts with such parameters" $ do
      let reqCase = req {queryString = [tagId]}
      let serverHandleCase = serverHandle {hPost = postHandle {Post.hGet = \_ _ _ _ _ -> return []}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" reqCase ""
      let expectation = Identity . showResp $ response400 noPost
      result `shouldBe` expectation

authorId :: QueryItem
authorId = ("author_id", Just "1")

categoryId :: QueryItem
categoryId = ("category_id", Just "1")

tag :: QueryItem
tag = ("tag", Just "tag")

tagId :: QueryItem
tagId = ("tag_id", Just "1")

tagIn :: QueryItem
tagIn = ("tag_in", Just "1,2")

tagAll :: QueryItem
tagAll = ("tag_all", Just "1,2")

postName :: QueryItem
postName = ("post_name", Just "name")

text :: QueryItem
text = ("text", Just "text")

substring :: QueryItem
substring = ("substring", Just "substring")

dateAfter :: QueryItem
dateAfter = ("date_after", Just "01-01-2000")

dateAt :: QueryItem
dateAt = ("date_at", Just "01-01-2000")

dateBefore :: QueryItem
dateBefore = ("date_before", Just "01-01-2000")

sortBy :: QueryItem
sortBy = ("sort_by", Just "by_author")
