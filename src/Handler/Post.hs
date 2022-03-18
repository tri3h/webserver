{-# LANGUAGE OverloadedStrings #-}
module Handler.Post where

import Types.Post
import Types.Tag
import qualified Types.Filter as F
import Data.Text ( Text )

data Handle m = Handle {
    get :: [PostId] -> m [Post],
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
    r1 <- applyFilter (F.dateAfter params) (filterByDateAfter handle)
    r2 <- applyFilter (F.dateBefore params) (filterByDateBefore handle)
    r3 <- applyFilter (F.dateAt params) (filterByDateAt handle)
    r4 <- applyFilter (F.authorName params) (filterByAuthorName handle)
    r5 <- applyFilter (F.categoryId params) (filterByCategoryId handle)
    r6 <- applyFilter (F.tagId params) (filterByTagId handle)
    r7 <- applyFilter (F.tag params) (filterByTag handle)
    r8 <- applyFilter (F.tagIn params) (filterByOneOfTags handle)
    r9 <- applyFilter (F.tagAll params) (filterByAllOfTags handle)
    r10 <- applyFilter (F.postName params) (filterByPostName handle)
    r11 <- applyFilter (F.text params) (filterByText handle)
    r12 <- applyFilter (F.substring params) (filterBySubstring handle)
    let common = chooseCommon $ filter (not . null) [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12]
    orderedCommon <- applyOrder [F.date order, F.author order,
                        F.category order, F.photosNumber order] common 
                        [orderByDate handle,orderByAuthor handle, 
                        orderByCategory handle, orderByPhotosNumber handle]
    if null orderedCommon
        then return $ Left "No posts with such parameters"
        else Right <$> get handle orderedCommon
    --добавить другие вложенные сущности

applyFilter Nothing _ = return []
applyFilter (Just x) f = f x

applyOrder [] xs _ = return xs
applyOrder _ xs [] = return xs
applyOrder (o:order) xs (f:funcs) = if o then f xs else applyOrder order xs funcs

chooseCommon :: Eq a => [[a]] -> [a]
chooseCommon [] = []
chooseCommon xs = foldl1 (\a b -> filter (`elem` b) a) xs
