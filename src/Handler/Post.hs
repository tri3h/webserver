{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Handler.Post(get, Handle (..), FilterHandle(..), OrderHandle (..)) where

import Types.Post
    ( Post(FullPost, ShortPost, authorId, categoryId, author, user, category, tag,
           comment, minorPhoto, postId, name, date, text, mainPhoto),
      PostId )
import Types.Tag(Tag, TagId)
import Types.Comment(Comment, CommentId)
import Types.User(User,UserId)
import Types.Category(Category,CategoryId)
import qualified Types.Author as A
import qualified Types.Filter as F
import Data.Text ( Text )
import Control.Monad ( void )
import Data.List(zipWith7)
import Types.Image (Image(Link))

data Handle m = Handle {
    hFilterHandle :: FilterHandle m,
    hOrderHandle :: OrderHandle m,
    hGet :: [PostId] -> F.Offset -> F.Limit -> m [Post],
    hGetAll :: m [PostId],
    hGetMinorPhotos :: PostId -> m [Image],
    hGetAuthor :: A.AuthorId -> m (Maybe A.Author),
    hGetUser :: UserId -> m (Maybe User),
    hGetCategory :: CategoryId -> m [Category],
    hGetTag :: PostId -> m [Tag],
    hGetComment :: PostId -> m [Comment]
}

data FilterHandle m = FilterHandle {
    hByDateBefore :: Text -> m [PostId],
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

data OrderHandle m = OrderHandle {
    hByDate :: [PostId] -> m [PostId],
    hByAuthor :: [PostId] -> m [PostId],
    hByCategory :: [PostId] -> m [PostId],
    hByPhotosNumber :: [PostId] -> m [PostId]
}

get :: Monad m => Handle m -> F.Filter -> F.Order -> F.Limit -> F.Offset -> m (Either Text [Post])
get handle params order limit offset = do
    orderedCommon <- getOrderedCommon handle params order
    posts <- hGet handle orderedCommon offset limit
    let isFormatCorrect = all (\case ShortPost {} -> True; _ -> False) posts
    if isFormatCorrect
    then do
        authors <- mapM (hGetAuthor handle . authorId) posts
        users <- mapM (\case Nothing -> return Nothing; Just a -> hGetUser handle $ A.userId a) authors
        categories <- mapM (hGetCategory handle . categoryId) posts
        tags <- mapM (hGetTag handle . postId) posts
        comments <- mapM (hGetComment handle . postId) posts
        minorPhotos <- mapM (hGetMinorPhotos handle . postId) posts
        let isMainPhotoCorrect = all (\x -> case mainPhoto x of Link {} -> True; _ -> False) posts
        let isMinorPhotoCorrect = all (all (\case Link {} -> True; _ -> False)) minorPhotos
        if isMainPhotoCorrect && isMinorPhotoCorrect 
        then do 
            let fullPosts = zipWith7 (\post author user category tag comment minorPhoto ->
                    FullPost {
                        postId = postId post,
                        name = name post,
                        date = date post,
                        text = text post,
                        mainPhoto = mainPhoto post,
                        ..} ) posts authors users categories tags comments minorPhotos
            if null orderedCommon
            then return $ Left "No posts with such parameters"
            else return $ Right fullPosts
        else return $ Left "Malformed images" 
    else return $ Left "Malformed posts"

getOrderedCommon :: Monad m => Handle m -> F.Filter -> F.Order -> m [PostId]
getOrderedCommon handle params order = do
    let isFiltersEmpty = all (==Nothing) [void $ F.dateAfter params,
            void $ F.dateBefore params, void $ F.dateAt params,
            void $ F.authorName params, void $ F.categoryId params,
            void $ F.tagId params, void $ F.tag params, void $ F.tagIn params,
            void $ F.tagAll params, void $ F.postName params,
            void $ F.text params, void $ F.substring params]
    common <- if isFiltersEmpty
            then hGetAll handle
            else chooseFiltered handle params
    case order of
        F.ByAuthor -> hByAuthor (hOrderHandle handle) common
        F.ByCategory -> hByCategory (hOrderHandle handle) common
        F.ByDate -> hByDate (hOrderHandle handle) common
        F.ByPhotosNumber -> hByPhotosNumber (hOrderHandle handle) common
        _ -> return common

chooseFiltered :: Monad m => Handle m -> F.Filter -> m [PostId]
chooseFiltered handle params= do
    filtered <- sequence
        [applyFilter (F.dateAfter params) (hByDateAfter $ hFilterHandle handle),
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
        applyFilter (F.substring params) (hBySubstring $ hFilterHandle handle)]
    let notNull = filter (not . null) filtered
    return $ chooseCommon notNull

applyFilter :: Monad m => Maybe t -> (t -> m [a]) -> m [a]
applyFilter Nothing _ = return []
applyFilter (Just x) f = f x

chooseCommon :: Eq a => [[a]] -> [a]
chooseCommon [] = []
chooseCommon xs = foldl1 (\a b -> filter (`elem` b) a) xs