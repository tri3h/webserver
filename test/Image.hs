{-# LANGUAGE OverloadedStrings #-}

module Image where

import Data.Functor.Identity (Identity (Identity))
import Error (noImage, noSpecified)
import Handlers (imageHandle, loggerHandle, serverHandle)
import qualified Handlers.Image as Image
import Handlers.Server (Handle (hImage), makeNoTokenResponse)
import Network.HTTP.Types (QueryItem)
import Network.Wai (Request (pathInfo, queryString, requestMethod), defaultRequest)
import Test.Hspec (describe, hspec, it, shouldBe)
import Utility (response200Image, response400, showResp)

main :: IO ()
main = hspec $
  describe "Testing get image" $ do
    let req =
          defaultRequest
            { pathInfo = ["images"],
              requestMethod = "GET",
              queryString =
                [ imageId
                ]
            }
    it "Should successfully get" $ do
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" req ""
      let expectation = Identity . showResp $ response200Image "" ""
      result `shouldBe` expectation
    it "Should fail if there is no image id" $ do
      let reqCase = req {queryString = []}
      let result = showResp <$> makeNoTokenResponse serverHandle loggerHandle "" reqCase ""
      let expectation = Identity . showResp . response400 $ noSpecified "image_id"
      result `shouldBe` expectation
    it "Should fail if image doesn't exist" $ do
      let serverHandleCase = serverHandle {hImage = imageHandle {Image.hGet = \_ -> return $ Left noImage}}
      let result = showResp <$> makeNoTokenResponse serverHandleCase loggerHandle "" req ""
      let expectation = Identity . showResp $ response400 noImage
      result `shouldBe` expectation

imageId :: QueryItem
imageId = ("image_id", Just "1")
