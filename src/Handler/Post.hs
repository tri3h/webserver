{-# LANGUAGE OverloadedStrings #-}
module Handler.Post where

import Types.Post
import Types.Tag(Tag, TagId)
import Types.Comment(Comment, CommentId)
import Types.User(User,UserId)
import Types.Category(Category,CategoryId)
import qualified Types.Author as A
import qualified Types.Filter as F
import Data.Text ( Text )
import Control.Monad
import Data.List(zipWith7)

data Handle m = Handle {
    get :: [PostId] -> m [Post],
    getMinorPhotos :: PostId -> m [Text],
    getAuthor :: A.AuthorId -> m A.Author,
    getUser :: UserId -> m User,
    getCategory :: CategoryId -> m [Category],
    getTag :: PostId -> m [Tag],
    getComment :: PostId -> m [Comment],
    filterByDateBefore :: Text -> m [PostId],
    filterByDateAfter :: Text -> m [PostId],
    filterByDateAt :: Text -> m [PostId],
    filterByAuthorName :: Text -> m [PostId],
    filterByCategoryId :: Integer -> m [PostId],
    filterByTagId :: TagId -> m [PostId],
    filterByTag :: Text -> m [PostId],
    filterByOneOfTags :: [TagId] -> m [PostId],
    filterByAllOfTags :: [TagId] -> m [PostId],
    filterByPostName :: Text -> m [PostId],
    filterByText :: Text -> m [PostId],
    filterBySubstring :: Text -> m [PostId],
    orderByDate :: [PostId] -> m [PostId],
    orderByAuthor :: [PostId] -> m [PostId],
    orderByCategory :: [PostId] -> m [PostId],
    orderByPhotosNumber :: [PostId] -> m [PostId]
}

getPost :: Monad m => Handle m -> F.Filter -> F.Order -> m (Either Text [Post])
getPost handle params order = do
    filtered <- sequence 
                    [applyFilter (F.dateAfter params) (filterByDateAfter handle),
                    applyFilter (F.dateBefore params) (filterByDateBefore handle),
                    applyFilter (F.dateAt params) (filterByDateAt handle),
                    applyFilter (F.authorName params) (filterByAuthorName handle),
                    applyFilter (F.categoryId params) (filterByCategoryId handle),
                    applyFilter (F.tagId params) (filterByTagId handle),
                    applyFilter (F.tag params) (filterByTag handle),
                    applyFilter (F.tagIn params) (filterByOneOfTags handle),
                    applyFilter (F.tagAll params) (filterByAllOfTags handle),
                    applyFilter (F.postName params) (filterByPostName handle),
                    applyFilter (F.text params) (filterByText handle),
                    applyFilter (F.substring params) (filterBySubstring handle)]
    let common = chooseCommon $ filter (not . null) filtered
    orderedCommon <- case order of 
        F.ByAuthor -> orderByAuthor handle common
        F.ByCategory -> orderByCategory handle common 
        F.ByDate -> orderByDate handle common 
        F.ByPhotosNumber -> orderByPhotosNumber handle common
        _ -> return common 
    posts <- get handle orderedCommon
    authors <- mapM (getAuthor handle . authorId) posts
    users <- mapM (getUser handle . A.userId) authors
    categories <- mapM (getCategory handle . categoryId) posts
    tags <- mapM (getTag handle . postId) posts
    comments <- mapM (getComment handle . postId) posts
    minorPhotos <- mapM (getMinorPhotos handle . postId) posts
    let fullPosts = zipWith7 (\p a u cat t com mp ->
            PostToGet {
                author = a,
                user = u,
                category = cat,
                tag = t,
                comment = com,
                minorPhoto = mp,
                postId = postId p,
                name = name p,
                date = date p,
                text = text p,
                mainPhoto = mainPhoto p
            } ) posts authors users categories tags comments minorPhotos
    if null common
        then return $ Left "No posts with such parameters"
        else return $ Right fullPosts

applyFilter Nothing _ = return []
applyFilter (Just x) f = f x

chooseCommon :: Eq a => [[a]] -> [a]
chooseCommon [] = []
chooseCommon xs = foldl1 (\a b -> filter (`elem` b) a) xs