{-# LANGUAGE OverloadedStrings #-}

module PostSpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text, append, pack)
import qualified Handlers.Post as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Config (ServerAddress)
import qualified Types.Filter as F
import Types.Image (Image (..), malformedImage, imageAddress)
import Types.Post (Post (..), malformedPost, noPost)
import Prelude hiding (filter)

main :: IO ()
main = hspec $
  describe "Testing get post" $ do
    it "Should successfully get without filters" $ do
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by date before" $ do
      let filterCase =
            filter
              { F.dateBefore = Just "10-10-2020"
              }
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by date after" $ do
      let filterCase =
            filter
              { F.dateAfter = Just "10-10-2020"
              }
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by date at" $ do
      let filterCase =
            filter
              { F.dateAt = Just "10-10-2020"
              }
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by author name" $ do
      let filterCase =
            filter
              { F.authorName = Just "name"
              }
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by category id" $ do
      let filterCase =
            filter
              { F.categoryId = Just 1
              }
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by tag id" $ do
      let filterCase =
            filter
              { F.tagId = Just 1
              }
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by tag" $ do
      let filterCase =
            filter
              { F.tag = Just "tag"
              }
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by one of tags" $ do
      let filterCase =
            filter
              { F.tagIn = Just [1, 2]
              }
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by all of tags" $ do
      let filterCase =
            filter
              { F.tagAll = Just [1, 2]
              }
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by post name" $ do
      let filterCase =
            filter
              { F.postName = Just "post"
              }
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by text" $ do
      let filterCase =
            filter
              { F.text = Just "text"
              }
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by substring" $ do
      let filterCase =
            filter
              { F.substring = Just "name"
              }
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should filter by several parameters" $ do
      let filterCase =
            filter
              { F.categoryId = Just 1,
                F.tagId = Just 1
              }
      let result = H.get handle serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should order by date" $ do
      let orderCase = F.ByDate
      let result = H.get handle serverAddress filter orderCase limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should order by author" $ do
      let orderCase = F.ByAuthor
      let result = H.get handle serverAddress filter orderCase limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should order by category" $ do
      let orderCase = F.ByCategory
      let result = H.get handle serverAddress filter orderCase limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should order by photos number" $ do
      let orderCase = F.ByPhotosNumber
      let result = H.get handle serverAddress filter orderCase limit offset
      result `shouldBe` return (Right [fullPost])
    it "Should get several posts" $ do
      let handleCase =
            handle
              { H.hGetAll = return [1, 1],
                H.hGet = \_ _ _ -> return [shortPost, shortPost]
              }
      let result = H.get handleCase serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost, fullPost])
    it "Should get no posts if there are no post with such parameters" $ do
      let filterHandleCase =
            filterHandle
              { H.hByCategoryId = \_ -> return []
              }
      let handleCase =
            handle
              { H.hGet = \_ _ _ -> return [],
                H.hFilterHandle = filterHandleCase
              }
      let filterCase =
            filter
              { F.categoryId = Just 1
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Left noPost)
    it "Should fail if post format is incorrect" $ do
      let handleCase =
            handle
              { H.hGet = \_ _ _ -> return [fullPost]
              }
      let result = H.get handleCase serverAddress filter order limit offset
      result `shouldBe` return (Left malformedPost)
    it "Should fail if main photo format is incorrect" $ do
      let shortPostCase =
            shortPost
              { mainPhoto = image
              }
      let handleCase =
            handle
              { H.hGet = \_ _ _ -> return [shortPostCase]
              }
      let result = H.get handleCase serverAddress filter order limit offset
      result `shouldBe` return (Left malformedImage)
    it "Should fail if minor photo format is incorrect" $ do
      let handleCase =
            handle
              { H.hGetMinorPhotos = \_ -> return [image]
              }
      let result = H.get handleCase serverAddress filter order limit offset
      result `shouldBe` return (Left malformedImage)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hFilterHandle = filterHandle,
      H.hOrderHandle = orderHandle,
      H.hGet = \_ _ _ -> return [shortPost],
      H.hGetAll = return [1],
      H.hGetMinorPhotos = \_ -> return [],
      H.hGetAuthor = \_ -> return Nothing,
      H.hGetUser = \_ -> return Nothing,
      H.hGetCategory = \_ -> return [],
      H.hGetTag = \_ -> return [],
      H.hGetComment = \_ -> return []
    }

filterHandle :: H.FilterHandle Identity
filterHandle =
  H.FilterHandle
    { H.hByDateBefore = \_ -> return [1],
      H.hByDateAfter = \_ -> return [1],
      H.hByDateAt = \_ -> return [1],
      H.hByAuthorName = \_ -> return [1],
      H.hByCategoryId = \_ -> return [1],
      H.hByTagId = \_ -> return [1],
      H.hByTag = \_ -> return [1],
      H.hByOneOfTags = \_ -> return [1],
      H.hByAllOfTags = \_ -> return [1],
      H.hByPostName = \_ -> return [1],
      H.hByText = \_ -> return [1],
      H.hBySubstring = \_ -> return [1]
    }

orderHandle :: H.OrderHandle Identity
orderHandle =
  H.OrderHandle
    { H.hByDate = \_ -> return [1],
      H.hByAuthor = \_ -> return [1],
      H.hByCategory = \_ -> return [1],
      H.hByPhotosNumber = \_ -> return [1]
    }

shortPost :: Post
shortPost =
  ShortPost
    { postId = 1,
      authorId = 1,
      categoryId = 1,
      name = "name",
      date = "01-01-2020",
      text = "text",
      mainPhoto = link
    }

fullPost :: Post
fullPost =
  FullPost
    { postId = 1,
      author = Nothing,
      user = Nothing,
      category = [],
      tag = [],
      comment = [],
      name = "name",
      date = "01-01-2020",
      text = "text",
      mainPhoto = link,
      minorPhoto = []
    }

filter :: F.Filter
filter =
  F.Filter
    { F.dateAfter = Nothing,
      F.dateBefore = Nothing,
      F.dateAt = Nothing,
      F.authorName = Nothing,
      F.categoryId = Nothing,
      F.tagId = Nothing,
      F.tag = Nothing,
      F.tagIn = Nothing,
      F.tagAll = Nothing,
      F.postName = Nothing,
      F.text = Nothing,
      F.substring = Nothing
    }

image :: Image
image = Image "image" "imageType"

link :: Image
link = Link $ imageAddress `append` pack (show imageId)

imageId :: Image
imageId = Id 1

limit :: F.Limit
limit = 5

offset :: F.Offset
offset = 0

order :: F.Order
order = F.None

serverAddress :: ServerAddress
serverAddress = ""
