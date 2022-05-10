{-# LANGUAGE OverloadedStrings #-}

module PostSpec where

import Data.Functor.Identity (Identity)
import Data.Text (append, pack)
import Error (noPost)
import qualified Handlers.Post as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Author (AuthorId (AuthorId))
import Types.Category (CategoryId (CategoryId))
import Types.Config (ServerAddress)
import qualified Types.Filter as F
import Types.Image (Image (..), ImageId (ImageId), Link (Link), imageAddress)
import Types.Post (Date (Date), FullPost (..), Name (Name), ShortPost (..))
import Types.PostComment (PostId (PostId))

main :: IO ()
main = hspec $
  describe "Testing get post" $ do
    it "Should successfully get" $ do
      let result = H.get handle serverAddress filters order limit offset
      result `shouldBe` return (Right [fullPost1, fullPost2])
    it "Should fail if there are no posts" $ do
      let handleCase =
            handle
              { H.hGet = \_ _ _ _ _ -> return []
              }
      let result = H.get handleCase serverAddress filters order limit offset
      result `shouldBe` return (Left noPost)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hGet = \_ _ _ _ _ -> return [shortPost1, shortPost2],
      H.hGetMinorPhotos = \_ _ -> return [],
      H.hGetAuthor = \_ -> return Nothing,
      H.hGetUser = \_ _ -> return Nothing,
      H.hGetCategory = \_ -> return [],
      H.hGetTag = \_ -> return [],
      H.hGetComment = \_ -> return []
    }

filters :: F.Filter
filters =
  F.Filter
    { F.dateAfter = Nothing,
      F.dateBefore = Nothing,
      F.dateAt = Nothing,
      F.authorName = Nothing,
      F.categoryId = Nothing,
      F.tagId = Nothing,
      F.tag = Nothing,
      F.tagIn = [],
      F.tagAll = [],
      F.postName = Nothing,
      F.text = Nothing,
      F.substring = Nothing
    }

shortPost1 :: ShortPost
shortPost1 =
  ShortPost
    { sPostId = postId1,
      sAuthorId = Just $ AuthorId 1,
      sCategoryId = Just $ CategoryId 1,
      sName = Name "post1",
      sDate = Date "01-01-2020",
      sText = "text",
      sMainPhoto = Just link
    }

fullPost1 :: FullPost
fullPost1 =
  FullPost
    { fPostId = postId1,
      fAuthor = Nothing,
      fUser = Nothing,
      fCategory = [],
      fTag = [],
      fComment = [],
      fName = Name "post1",
      fDate = Date "01-01-2020",
      fText = "text",
      fMainPhoto = Just link,
      fMinorPhoto = []
    }

shortPost2 :: ShortPost
shortPost2 =
  ShortPost
    { sPostId = postId2,
      sAuthorId = Just $ AuthorId 2,
      sCategoryId = Just $ CategoryId 2,
      sName = Name "post2",
      sDate = Date "01-01-2022",
      sText = "text",
      sMainPhoto = Just link
    }

fullPost2 :: FullPost
fullPost2 =
  FullPost
    { fPostId = postId2,
      fAuthor = Nothing,
      fUser = Nothing,
      fCategory = [],
      fTag = [],
      fComment = [],
      fName = Name "post2",
      fDate = Date "01-01-2022",
      fText = "text",
      fMainPhoto = Just link,
      fMinorPhoto = []
    }

postId1 :: PostId
postId1 = PostId 1

postId2 :: PostId
postId2 = PostId 2

image :: Image
image = Image "image" "imageType"

link :: Link
link = Link $ imageAddress `append` pack (show imageId)

imageId :: ImageId
imageId = ImageId 1

limit :: F.Limit
limit = F.Limit 5

offset :: F.Offset
offset = F.Offset 0

order :: F.Order
order = F.None

serverAddress :: ServerAddress
serverAddress = ""
