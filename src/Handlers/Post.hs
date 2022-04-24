{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Post (get, Handle (..), FilterHandle (..), OrderHandle (..)) where

import Control.Monad (void)
import Data.List (elemIndex, zipWith7)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Types.Author as A
import Types.Category (CategoryId, GetCategory)
import Types.Comment (GetComment)
import Types.Config (ServerAddress)
import qualified Types.Filter as F
import Types.Image (Image)
import Types.Post
  ( Date,
    FullPost (..),
    Name,
    ShortPost (..),
    noPost,
  )
import Types.PostComment (PostId)
import qualified Types.Tag as Tag
import qualified Types.User as User
import Utility (imagesToLinks)

data Handle m = Handle
  { hFilterHandle :: FilterHandle m,
    hOrderHandle :: OrderHandle m,
    hGet :: [PostId] -> m [ShortPost],
    hGetAll :: m [PostId],
    hGetMinorPhotos :: PostId -> m [Image],
    hGetAuthor :: Maybe A.AuthorId -> m (Maybe A.GetAuthor),
    hGetUser :: Maybe User.UserId -> m (Maybe User.GetUser),
    hGetCategory :: Maybe CategoryId -> m [GetCategory],
    hGetTag :: PostId -> m [Tag.Tag],
    hGetComment :: PostId -> m [GetComment],
    hApplyLimitOffset :: [PostId] -> F.Limit -> F.Offset -> m [PostId]
  }

data FilterHandle m = FilterHandle
  { hByDateBefore :: Date -> m [PostId],
    hByDateAfter :: Date -> m [PostId],
    hByDateAt :: Date -> m [PostId],
    hByAuthorName :: User.Name -> m [PostId],
    hByCategoryId :: CategoryId -> m [PostId],
    hByTagId :: Tag.TagId -> m [PostId],
    hByTag :: Tag.Name -> m [PostId],
    hByOneOfTags :: [Tag.TagId] -> m [PostId],
    hByAllOfTags :: [Tag.TagId] -> m [PostId],
    hByPostName :: Name -> m [PostId],
    hByText :: Text -> m [PostId],
    hBySubstring :: Text -> m [PostId]
  }

data OrderHandle m = OrderHandle
  { hByDate :: [PostId] -> m [PostId],
    hByAuthor :: [PostId] -> m [PostId],
    hByCategory :: [PostId] -> m [PostId],
    hByPhotosNumber :: [PostId] -> m [PostId]
  }

get :: Monad m => Handle m -> ServerAddress -> F.Filter -> F.Order -> F.Limit -> F.Offset -> m (Either Text [FullPost])
get handle server params order limit offset = do
  let isFiltersEmpty =
        all
          (== Nothing)
          [ void $ F.dateAfter params,
            void $ F.dateBefore params,
            void $ F.dateAt params,
            void $ F.authorName params,
            void $ F.categoryId params,
            void $ F.tagId params,
            void $ F.tag params,
            void $ F.tagIn params,
            void $ F.tagAll params,
            void $ F.postName params,
            void $ F.text params,
            void $ F.substring params
          ]
  common <-
    if isFiltersEmpty
      then hGetAll handle
      else getFiltered handle params
  ordered <- getOrdered handle common order
  limitedOffseted <- hApplyLimitOffset handle ordered limit offset
  if null limitedOffseted
    then return $ Left noPost
    else do
      posts <- hGet handle limitedOffseted
      makeFullPost handle server posts limitedOffseted

orderFullPost :: [PostId] -> [FullPost] -> [FullPost]
orderFullPost orderedId posts =
  let p = map fPostId posts
      index = map (fromJust . (`elemIndex` p)) orderedId
   in map (posts !!) index

makeFullPost :: Monad m => Handle m -> ServerAddress -> [ShortPost] -> [PostId] -> m (Either Text [FullPost])
makeFullPost handle server posts orderedId = do
  authors <- mapM (hGetAuthor handle . sAuthorId) posts
  users <- mapM (\case Nothing -> return Nothing; Just a -> hGetUser handle $ A.gUserId a) authors
  categories <- mapM (hGetCategory handle . sCategoryId) posts
  tags <- mapM (hGetTag handle . sPostId) posts
  comments <- mapM (hGetComment handle . sPostId) posts
  minorPhotos <- mapM (hGetMinorPhotos handle . sPostId) posts
  let mainPhotoLink = imagesToLinks server $ map sMainPhoto posts
  let minorPhotoLink = mapM (imagesToLinks server) minorPhotos
  case mainPhotoLink of
    Right mainLinks ->
      let postsWithLinks = zipWith (\post link -> post {sMainPhoto = link}) posts mainLinks
       in case minorPhotoLink of
            Right minorLinks -> do
              let fullPosts =
                    zipWith7
                      ( \post fAuthor fUser fCategory fTag fComment fMinorPhoto ->
                          FullPost
                            { fPostId = sPostId post,
                              fName = sName post,
                              fDate = sDate post,
                              fText = sText post,
                              fMainPhoto = sMainPhoto post,
                              ..
                            }
                      )
                      postsWithLinks
                      authors
                      users
                      categories
                      tags
                      comments
                      minorLinks
              return . Right $ orderFullPost orderedId fullPosts
            Left l -> return $ Left l
    Left l -> return $ Left l

getOrdered :: Monad m => Handle m -> [PostId] -> F.Order -> m [PostId]
getOrdered handle posts order =
  case order of
    F.ByAuthor -> hByAuthor (hOrderHandle handle) posts
    F.ByCategory -> hByCategory (hOrderHandle handle) posts
    F.ByDate -> hByDate (hOrderHandle handle) posts
    F.ByPhotosNumber -> hByPhotosNumber (hOrderHandle handle) posts
    _ -> return posts

getFiltered :: Monad m => Handle m -> F.Filter -> m [PostId]
getFiltered handle params = do
  filtered <-
    sequence
      [ applyFilter (F.dateAfter params) (hByDateAfter $ hFilterHandle handle),
        applyFilter (F.dateBefore params) (hByDateBefore $ hFilterHandle handle),
        applyFilter (F.dateAt params) (hByDateAt $ hFilterHandle handle),
        applyFilter (F.authorName params) (hByAuthorName $ hFilterHandle handle),
        applyFilter (F.categoryId params) (hByCategoryId $ hFilterHandle handle),
        applyFilter (F.tagId params) (hByTagId $ hFilterHandle handle),
        applyFilter (F.tag params) (hByTag $ hFilterHandle handle),
        applyFilter (F.tagIn params) (hByOneOfTags $ hFilterHandle handle),
        applyFilter (F.tagAll params) (hByAllOfTags $ hFilterHandle handle),
        applyFilter (F.postName params) (hByPostName $ hFilterHandle handle),
        applyFilter (F.text params) (hByText $ hFilterHandle handle),
        applyFilter (F.substring params) (hBySubstring $ hFilterHandle handle)
      ]
  let notNull = filter (not . null) filtered
  return $ chooseCommon notNull

applyFilter :: Monad m => Maybe t -> (t -> m [a]) -> m [a]
applyFilter Nothing _ = return []
applyFilter (Just x) f = f x

chooseCommon :: Eq a => [[a]] -> [a]
chooseCommon [] = []
chooseCommon xs = foldl1 (\a b -> filter (`elem` b) a) xs
