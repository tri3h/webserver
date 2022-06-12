{-# LANGUAGE OverloadedStrings #-}

module Post where

import Data (authorId, categoryId, dateAfter, dateBefore, postName, sortBy, substring, tag, tagAll, tagId, tagIn, text)
import Data.Functor.Identity (Identity (Identity))
import Error (noPost)
import Handlers (fullPost, loggerHandle, postHandle, serverHandle)
import qualified Handlers.Post as Post
import Handlers.Server (Handle (hPost), makeNoTokenResponse)
import Network.Wai (Request (pathInfo, queryString, requestMethod), defaultRequest)
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Post (FullPost)
import Utility (response200JSON, response400, showResp)

test :: IO ()
test = hspec $
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
