{-# LANGUAGE OverloadedStrings #-}

module PostSpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text, append, pack)
import qualified Handlers.Post as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Config (ServerAddress)
import qualified Types.Filter as F
import Types.Image (Image (..), imageAddress, malformedImage)
import Types.Post (Post (..), malformedPost, noPost)
import Prelude hiding (filter)
import qualified Prelude

main :: IO ()
main = hspec $
  describe "Testing get post" $ do
    it "Should successfully get without filters" $ do
      let result = H.get handle serverAddress filter order limit offset
      result `shouldBe` return (Right [fullPost1, fullPost2])
    it "Should filter by date before" $ do
      let filterCase =
            filter
              { F.dateBefore = Just "10-10-2020"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByDateBefore = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filter by date after" $ do
      let filterCase =
            filter
              { F.dateAfter = Just "10-10-2020"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByDateAfter = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filter by date at" $ do
      let filterCase =
            filter
              { F.dateAt = Just "10-10-2020"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByDateAt = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filter by author name" $ do
      let filterCase =
            filter
              { F.authorName = Just "name"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByAuthorName = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filter by category id" $ do
      let filterCase =
            filter
              { F.categoryId = Just 1
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByCategoryId = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filter by tag id" $ do
      let filterCase =
            filter
              { F.tagId = Just 1
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByTagId = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filter by tag" $ do
      let filterCase =
            filter
              { F.tag = Just "tag"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByTag = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filter by one of tags" $ do
      let filterCase =
            filter
              { F.tagIn = Just [1, 2]
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByOneOfTags = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filter by all of tags" $ do
      let filterCase =
            filter
              { F.tagAll = Just [1, 2]
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByAllOfTags = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filter by post name" $ do
      let filterCase =
            filter
              { F.postName = Just "post"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByPostName = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filter by text" $ do
      let filterCase =
            filter
              { F.text = Just "text"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByText = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filter by substring" $ do
      let filterCase =
            filter
              { F.substring = Just "name"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hBySubstring = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filter by several parameters" $ do
      let filterCase =
            filter
              { F.categoryId = Just 1,
                F.tagId = Just 1
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByCategoryId = \_ -> return [1],
                      H.hByTagId = \_ -> return [1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should order by date" $ do
      let orderCase = F.ByDate
      let handleCase =
            handle
              { H.hOrderHandle =
                  orderHandle
                    { H.hByDate = \_ -> return [2, 1]
                    }
              }
      let result = H.get handleCase serverAddress filter orderCase limit offset
      result `shouldBe` return (Right [fullPost2, fullPost1])
    it "Should order by author" $ do
      let orderCase = F.ByAuthor
      let handleCase =
            handle
              { H.hOrderHandle =
                  orderHandle
                    { H.hByAuthor = \_ -> return [2, 1]
                    }
              }
      let result = H.get handleCase serverAddress filter orderCase limit offset
      result `shouldBe` return (Right [fullPost2, fullPost1])
    it "Should order by category" $ do
      let orderCase = F.ByCategory
      let handleCase =
            handle
              { H.hOrderHandle =
                  orderHandle
                    { H.hByCategory = \_ -> return [2, 1]
                    }
              }
      let result = H.get handleCase serverAddress filter orderCase limit offset
      result `shouldBe` return (Right [fullPost2, fullPost1])
    it "Should order by photos number" $ do
      let orderCase = F.ByPhotosNumber
      let handleCase =
            handle
              { H.hOrderHandle =
                  orderHandle
                    { H.hByPhotosNumber = \_ -> return [2, 1]
                    }
              }
      let result = H.get handleCase serverAddress filter orderCase limit offset
      result `shouldBe` return (Right [fullPost2, fullPost1])
    it "Should apply limit" $ do
      let limitCase = 1
      let handleCase =
            handle
              { H.hApplyLimitOffset = \_ _ _ -> return [1]
              }
      let result = H.get handleCase serverAddress filter order limitCase offset
      result `shouldBe` return (Right [fullPost1])
    it "Should apply offset" $ do
      let offsetCase = 1
      let handleCase =
            handle
              { H.hApplyLimitOffset = \_ _ _ -> return [2]
              }
      let result = H.get handleCase serverAddress filter order limit offsetCase
      result `shouldBe` return (Right [fullPost2])
    it "Should get no posts if there are no post with such parameters" $ do
      let filterCase =
            filter
              { F.categoryId = Just 1
              }
      let result = H.get handle serverAddress filterCase order limit offset
      result `shouldBe` return (Left noPost)
    it "Should fail if post format is incorrect" $ do
      let handleCase =
            handle
              { H.hGet = \_ -> return [fullPost1]
              }
      let result = H.get handleCase serverAddress filter order limit offset
      result `shouldBe` return (Left malformedPost)
    it "Should fail if main photo format is incorrect" $ do
      let shortPostCase =
            shortPost1
              { mainPhoto = image
              }
      let handleCase =
            handle
              { H.hGet = \_ -> return [shortPostCase]
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
      H.hGet = \posts -> return $ Prelude.filter (\a -> postId a `elem` posts) [shortPost1, shortPost2],
      H.hGetAll = return [1, 2],
      H.hGetMinorPhotos = \_ -> return [],
      H.hGetAuthor = \_ -> return Nothing,
      H.hGetUser = \_ -> return Nothing,
      H.hGetCategory = \_ -> return [],
      H.hGetTag = \_ -> return [],
      H.hGetComment = \_ -> return [],
      H.hApplyLimitOffset = \posts _ _ -> return posts
    }

filterHandle :: H.FilterHandle Identity
filterHandle =
  H.FilterHandle
    { H.hByDateBefore = \_ -> return [],
      H.hByDateAfter = \_ -> return [],
      H.hByDateAt = \_ -> return [],
      H.hByAuthorName = \_ -> return [],
      H.hByCategoryId = \_ -> return [],
      H.hByTagId = \_ -> return [],
      H.hByTag = \_ -> return [],
      H.hByOneOfTags = \_ -> return [],
      H.hByAllOfTags = \_ -> return [],
      H.hByPostName = \_ -> return [],
      H.hByText = \_ -> return [],
      H.hBySubstring = \_ -> return []
    }

orderHandle :: H.OrderHandle Identity
orderHandle =
  H.OrderHandle
    { H.hByDate = return,
      H.hByAuthor = return,
      H.hByCategory = return,
      H.hByPhotosNumber = return
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

shortPost1 :: Post
shortPost1 =
  ShortPost
    { postId = 1,
      authorId = 1,
      categoryId = 1,
      name = "post1",
      date = "01-01-2020",
      text = "text",
      mainPhoto = link
    }

fullPost1 :: Post
fullPost1 =
  FullPost
    { postId = 1,
      author = Nothing,
      user = Nothing,
      category = [],
      tag = [],
      comment = [],
      name = "post1",
      date = "01-01-2020",
      text = "text",
      mainPhoto = link,
      minorPhoto = []
    }

shortPost2 :: Post
shortPost2 =
  ShortPost
    { postId = 2,
      authorId = 2,
      categoryId = 2,
      name = "post2",
      date = "01-01-2022",
      text = "text",
      mainPhoto = link
    }

fullPost2 :: Post
fullPost2 =
  FullPost
    { postId = 2,
      author = Nothing,
      user = Nothing,
      category = [],
      tag = [],
      comment = [],
      name = "post2",
      date = "01-01-2022",
      text = "text",
      mainPhoto = link,
      minorPhoto = []
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
