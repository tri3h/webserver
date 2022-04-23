{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Post (get, Handle (..), FilterHandle (..), OrderHandle (..)) where

import Control.Monad (void)
import Data.List (elemIndex, zipWith7)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Types.Author as A
import Types.Category (Category, CategoryId)
import Types.Comment (Comment)
import Types.Config (ServerAddress)
import qualified Types.Filter as F
import Types.Image (Image)
import Types.Post
  ( Post
      ( FullPost,
        ShortPost,
        author,
        authorId,
        category,
        categoryId,
        comment,
        date,
        mainPhoto,
        minorPhoto,
        name,
        postId,
        tag,
        text,
        user
      ),
    PostId,
    malformedPost,
    noPost,
  )
import Types.Tag (Tag, TagId)
import Types.User (User, UserId)
import Utility (imagesToLinks)

data Handle m = Handle
  { hFilterHandle :: FilterHandle m,
    hOrderHandle :: OrderHandle m,
    hGet :: [PostId] -> m [Post],
    hGetAll :: m [PostId],
    hGetMinorPhotos :: PostId -> m [Image],
    hGetAuthor :: A.AuthorId -> m (Maybe A.Author),
    hGetUser :: UserId -> m (Maybe User),
    hGetCategory :: CategoryId -> m [Category],
    hGetTag :: PostId -> m [Tag],
    hGetComment :: PostId -> m [Comment],
    applyLimitOffset :: [PostId] -> F.Limit -> F.Offset -> m [PostId]
  }

data FilterHandle m = FilterHandle
  { hByDateBefore :: Text -> m [PostId],
    hByDateAfter :: Text -> m [PostId],
    hByDateAt :: Text -> m [PostId],
    hByAuthorName :: Text -> m [PostId],
    hByCategoryId :: Integer -> m [PostId],
    hByTagId :: TagId -> m [PostId],
    hByTag :: Text -> m [PostId],
    hByOneOfTags :: [TagId] -> m [PostId],
    hByAllOfTags :: [TagId] -> m [PostId],
    hByPostName :: Text -> m [PostId],
    hByText :: Text -> m [PostId],
    hBySubstring :: Text -> m [PostId]
  }

data OrderHandle m = OrderHandle
  { hByDate :: [PostId] -> m [PostId],
    hByAuthor :: [PostId] -> m [PostId],
    hByCategory :: [PostId] -> m [PostId],
    hByPhotosNumber :: [PostId] -> m [PostId]
  }

get :: Monad m => Handle m -> ServerAddress -> F.Filter -> F.Order -> F.Limit -> F.Offset -> m (Either Text [Post])
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
  limitedOffseted <- applyLimitOffset handle ordered limit offset
  if null limitedOffseted
    then return $ Left noPost
    else do
      posts <- hGet handle limitedOffseted
      let isFormatCorrect = all (\case ShortPost {} -> True; _ -> False) posts
      if isFormatCorrect
        then makeFullPost handle server posts limitedOffseted
        else return $ Left malformedPost

orderFullPost :: [PostId] -> [Post] -> [Post]
orderFullPost orderedId posts =
  let p = map postId posts
      index = map (fromJust . (`elemIndex` p)) orderedId
   in map (posts !!) index

makeFullPost :: Monad m => Handle m -> ServerAddress -> [Post] -> [PostId] -> m (Either Text [Post])
makeFullPost handle server posts orderedId = do
  authors <- mapM (hGetAuthor handle . authorId) posts
  users <- mapM (\case Nothing -> return Nothing; Just a -> hGetUser handle $ A.userId a) authors
  categories <- mapM (hGetCategory handle . categoryId) posts
  tags <- mapM (hGetTag handle . postId) posts
  comments <- mapM (hGetComment handle . postId) posts
  minorPhotos <- mapM (hGetMinorPhotos handle . postId) posts
  let mainPhotoLink = imagesToLinks server $ map mainPhoto posts
  let minorPhotoLink = mapM (imagesToLinks server) minorPhotos
  case mainPhotoLink of
    Right mainLinks ->
      let postsWithLinks = zipWith (\post link -> post {mainPhoto = link}) posts mainLinks
       in case minorPhotoLink of
            Right minorLinks -> do
              let fullPosts =
                    zipWith7
                      ( \post author user category tag comment minorPhoto ->
                          FullPost
                            { postId = postId post,
                              name = name post,
                              date = date post,
                              text = text post,
                              mainPhoto = mainPhoto post,
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
