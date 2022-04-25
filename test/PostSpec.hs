{-# LANGUAGE OverloadedStrings #-}

module PostSpec where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text, append, pack)
import qualified Handlers.Post as H
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Author (AuthorId (AuthorId))
import Types.Category (CategoryId (CategoryId))
import Types.Config (ServerAddress)
import qualified Types.Filter as F
import Types.Image (Image (..), imageAddress, malformedImage)
import Types.Post (Date (Date), FullPost (..), Name (Name), ShortPost (..), noPost)
import Types.PostComment (PostId (PostId))
import Types.Tag (TagId (TagId))
import qualified Types.Tag as Tag
import qualified Types.User as User

main :: IO ()
main = hspec $
  describe "Testing get post" $ do
    it "Should successfully get without filters" $ do
      let result = H.get handle serverAddress filters order limit offset
      result `shouldBe` return (Right [fullPost1, fullPost2])
    it "Should filters by date before" $ do
      let filterCase =
            filters
              { F.dateBefore = Just $ Date "10-10-2020"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByDateBefore = \_ -> return [postId1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filters by date after" $ do
      let filterCase =
            filters
              { F.dateAfter = Just $ Date "10-10-2020"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByDateAfter = \_ -> return [postId1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filters by date at" $ do
      let filterCase =
            filters
              { F.dateAt = Just $ Date "10-10-2020"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByDateAt = \_ -> return [postId1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filters by author name" $ do
      let filterCase =
            filters
              { F.authorName = Just $ User.Name "name"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByAuthorName = \_ -> return [postId1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filters by category id" $ do
      let filterCase =
            filters
              { F.categoryId = Just $ CategoryId 1
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByCategoryId = \_ -> return [postId1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filters by tag id" $ do
      let filterCase =
            filters
              { F.tagId = Just $ TagId 1
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByTagId = \_ -> return [postId1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filters by tag" $ do
      let filterCase =
            filters
              { F.tag = Just $ Tag.Name "tag"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByTag = \_ -> return [postId1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filters by one of tags" $ do
      let filterCase =
            filters
              { F.tagIn = Just [TagId 1, TagId 2]
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByOneOfTags = \_ -> return [postId1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filters by all of tags" $ do
      let filterCase =
            filters
              { F.tagAll = Just [TagId 1, TagId 2]
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByAllOfTags = \_ -> return [postId1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filters by post name" $ do
      let filterCase =
            filters
              { F.postName = Just $ Name "post"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByPostName = \_ -> return [postId1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filters by text" $ do
      let filterCase =
            filters
              { F.text = Just "text"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByText = \_ -> return [postId1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filters by substring" $ do
      let filterCase =
            filters
              { F.substring = Just "name"
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hBySubstring = \_ -> return [postId1]
                    }
              }
      let result = H.get handleCase serverAddress filterCase order limit offset
      result `shouldBe` return (Right [fullPost1])
    it "Should filters by several parameters" $ do
      let filterCase =
            filters
              { F.categoryId = Just $ CategoryId 1,
                F.tagId = Just $ TagId 1
              }
      let handleCase =
            handle
              { H.hFilterHandle =
                  filterHandle
                    { H.hByCategoryId = \_ -> return [postId1],
                      H.hByTagId = \_ -> return [postId1]
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
                    { H.hByDate = \_ -> return [postId2, postId1]
                    }
              }
      let result = H.get handleCase serverAddress filters orderCase limit offset
      result `shouldBe` return (Right [fullPost2, fullPost1])
    it "Should order by author" $ do
      let orderCase = F.ByAuthor
      let handleCase =
            handle
              { H.hOrderHandle =
                  orderHandle
                    { H.hByAuthor = \_ -> return [postId2, postId1]
                    }
              }
      let result = H.get handleCase serverAddress filters orderCase limit offset
      result `shouldBe` return (Right [fullPost2, fullPost1])
    it "Should order by category" $ do
      let orderCase = F.ByCategory
      let handleCase =
            handle
              { H.hOrderHandle =
                  orderHandle
                    { H.hByCategory = \_ -> return [postId2, postId1]
                    }
              }
      let result = H.get handleCase serverAddress filters orderCase limit offset
      result `shouldBe` return (Right [fullPost2, fullPost1])
    it "Should order by photos number" $ do
      let orderCase = F.ByPhotosNumber
      let handleCase =
            handle
              { H.hOrderHandle =
                  orderHandle
                    { H.hByPhotosNumber = \_ -> return [postId2, postId1]
                    }
              }
      let result = H.get handleCase serverAddress filters orderCase limit offset
      result `shouldBe` return (Right [fullPost2, fullPost1])
    it "Should apply limit" $ do
      let limitCase = F.Limit 1
      let handleCase =
            handle
              { H.hApplyLimitOffset = \_ _ _ -> return [postId1]
              }
      let result = H.get handleCase serverAddress filters order limitCase offset
      result `shouldBe` return (Right [fullPost1])
    it "Should apply offset" $ do
      let offsetCase = F.Offset 1
      let handleCase =
            handle
              { H.hApplyLimitOffset = \_ _ _ -> return [postId2]
              }
      let result = H.get handleCase serverAddress filters order limit offsetCase
      result `shouldBe` return (Right [fullPost2])
    it "Should get no posts if there are no post with such parameters" $ do
      let filterCase =
            filters
              { F.categoryId = Just $ CategoryId 1
              }
      let result = H.get handle serverAddress filterCase order limit offset
      result `shouldBe` return (Left noPost)
    it "Should fail if main photo format is incorrect" $ do
      let shortPostCase =
            shortPost1
              { sMainPhoto = image
              }
      let handleCase =
            handle
              { H.hGet = \_ -> return [shortPostCase]
              }
      let result = H.get handleCase serverAddress filters order limit offset
      result `shouldBe` return (Left malformedImage)
    it "Should fail if minor photo format is incorrect" $ do
      let handleCase =
            handle
              { H.hGetMinorPhotos = \_ -> return [image]
              }
      let result = H.get handleCase serverAddress filters order limit offset
      result `shouldBe` return (Left malformedImage)

handle :: H.Handle Identity
handle =
  H.Handle
    { H.hFilterHandle = filterHandle,
      H.hOrderHandle = orderHandle,
      H.hGet = \posts -> return $ filter (\a -> sPostId a `elem` posts) [shortPost1, shortPost2],
      H.hGetAll = return [postId1, postId2],
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
      F.tagIn = Nothing,
      F.tagAll = Nothing,
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
      sMainPhoto = link
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
      fMainPhoto = link,
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
      sMainPhoto = link
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
      fMainPhoto = link,
      fMinorPhoto = []
    }

postId1 :: PostId
postId1 = PostId 1

postId2 :: PostId
postId2 = PostId 2

image :: Image
image = Image "image" "imageType"

link :: Image
link = Link $ imageAddress `append` pack (show imageId)

imageId :: Image
imageId = Id 1

limit :: F.Limit
limit = F.Limit 5

offset :: F.Offset
offset = F.Offset 0

order :: F.Order
order = F.None

serverAddress :: ServerAddress
serverAddress = ""
