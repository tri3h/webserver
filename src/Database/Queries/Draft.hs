{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Queries.Draft where

import Database.PostgreSQL.Simple
    ( Connection, execute, executeMany, query, Only(Only) )
import Database.PostgreSQL.Simple.Time (Date)
import Database.Connection ( serverAddress )
import Data.Text(Text, append, pack)
import Types.Draft ( Description, Draft(..), DraftId, Name, draftNotExist )
import Types.Post (PostId)
import Types.Category(CategoryId)
import Types.Tag(TagId)
import Types.Image ( Image(..), ImageId )
import Data.Maybe ( fromMaybe )

create :: Draft -> Connection -> IO ()
create draft conn = do
    let (Image image imageType) = mainPhoto draft
    [Only imageId] <- query conn "INSERT INTO images (image, image_type) \
        \VALUES (decode(?,'base64'), ?) RETURNING image_id" (image, imageType)
    [Only draftId] <- query conn "INSERT INTO drafts \
        \(category_id, name, text, image_id, author_id) VALUES (?,?,?,?,?) \
        \RETURNING draft_id" (categoryId draft, name draft, description draft, imageId :: Integer, authorId draft)
    let tags = map (\x -> (draftId, x)) $ tagId draft
    executeMany conn "INSERT INTO draft_tags (draft_id, tag_id) \
        \VALUES (?,?)" (tags :: [(Integer,Integer)])
    return ()

get :: DraftId -> Connection -> IO Draft
get draftId conn = do
    server <- serverAddress
    [(draftId, postId, maybeAuthorId, maybeCategoryId, name, description, mainPhotoId)] <-
        query conn "SELECT draft_id, post_id, author_id, category_id, name, \
        \text, image_id FROM drafts WHERE draft_id = ?" (Only draftId)
    tagIds <- query conn "SELECT tag_id FROM draft_tags WHERE draft_id = ?" (Only draftId)
    minorPhotoId <- query conn "SELECT image_id \
        \FROM draft_minor_photos WHERE draft_id = ?" (Only draftId)
    return $ Draft {
        tagId = map (\(Only x) -> x) tagIds,
        mainPhoto = Link $ server `append` "/images?image_id=" 
            `append` pack (show (mainPhotoId :: Integer)),
        minorPhoto = map (\(Only x) -> Link $ server `append` "/images?image_id=" 
            `append` pack (show (x :: Integer))) minorPhotoId,
        authorId = fromMaybe 0 maybeAuthorId,
        categoryId = fromMaybe 0 maybeCategoryId,
        .. }

editCategoryId :: DraftId -> CategoryId -> Connection -> IO ()
editCategoryId draftId catId conn = do
    execute conn "UPDATE drafts SET category_id = ? WHERE draft_id = ?" (catId, draftId)
    return ()

editTagId :: DraftId -> [TagId] -> Connection -> IO ()
editTagId draftId tagIds conn = do
    let draftTags = map (\x -> (draftId, x)) tagIds
    execute conn "DELETE FROM draft_tags dt INNER JOIN drafts d \
        \ON dt.draft_id = d.draft_id WHERE draft_id = ?" (Only draftId)
    executeMany conn "INSERT INTO draft_tags (draft_id, tag_id) VALUES (?,?)" draftTags
    return ()

editName :: DraftId -> Name -> Connection -> IO ()
editName draftId name conn = do
    execute conn "UPDATE drafts SET name = ? WHERE draft_id = ?" (name, draftId)
    return ()

editDescription :: DraftId -> Description -> Connection -> IO ()
editDescription draftId descr conn = do
    execute conn "UPDATE drafts SET text = ? WHERE draft_id = ?" (descr, draftId)
    return ()

editMainPhoto :: DraftId -> Image -> Connection -> IO ()
editMainPhoto draftId photo conn = do
    [Only imageId] <- query conn "INSERT INTO images (image) \
        \VALUES (decode(?,'base64')) RETURNING image_id" (Only photo)
    execute conn "UPDATE drafts SET image_id = ? \
        \WHERE draft_id = ?" (imageId :: Integer, draftId)
    return ()

deleteMinorPhoto :: DraftId -> ImageId -> Connection -> IO ()
deleteMinorPhoto draftId photoId conn = do 
    execute conn "DELETE FROM draft_minor_photos \
        \WHERE image_id = ? AND draft_id = ?" (photoId, draftId)
    return ()

addMinorPhoto :: DraftId -> Image -> Connection -> IO ()
addMinorPhoto draftId photo conn = do 
    let (Image image imageType) = photo
    [Only imageId] <- query conn "INSERT INTO images (image, image_type) \
        \VALUES (?,?) RETURNING image_id" (image, imageType)
    execute conn "INSERT INTO draft_minor_photos (image_id, draft_id) \
        \VALUES (?,?)" (imageId :: Integer, draftId)
    return ()

delete :: DraftId -> Connection -> IO ()
delete draftId conn = do
    execute conn "DELETE FROM drafts WHERE drafts.draft_id = ?" (Only draftId)
    return ()

publish :: Draft -> String -> Connection -> IO ()
publish draft date conn = do
    [Only postId] <- query conn "INSERT INTO posts (author_id, category_id, \
        \name, date, text) VALUES (?,?,?,?,?) RETURNING post_id" (authorId draft, categoryId draft,
        name draft, date, description draft)
    [Only mainPhotoId] <- query conn "SELECT image_id FROM drafts WHERE draft_id = ?" (Only $ draftId draft)
    execute conn "UPDATE posts SET image_id = ? WHERE post_id = ?" (mainPhotoId :: Integer, postId)
    photoIds <- query conn "SELECT image_id FROM draft_minor_photos WHERE draft_id = ?" (Only $ draftId draft)
    let postPhotos = map (\(Only y) -> (postId :: Integer, y)) (photoIds :: [Only Integer])
    executeMany conn "INSERT INTO post_minor_photos (post_id, image_id) \
        \VALUES (?,?)" postPhotos
    let postTags = map (\x -> (postId, x)) $ tagId draft
    executeMany conn "INSERT INTO post_tags (post_id, tag_id) VALUES (?,?)" postTags
    return ()

update :: Draft -> Connection -> IO ()
update draft conn = do
    execute conn "DELETE FROM post_tags WHERE post_id = ?" (Only $ postId draft)
    execute conn "DELETE FROM post_minor_photos WHERE post_id = ?" (Only $ postId draft)
    photoIds <- query conn "SELECT image_id FROM draft_minor_photos WHERE draft_id = ?" (Only $ draftId draft)
    let postPhotos = map (\(Only y) -> (postId draft, y)) (photoIds :: [Only Integer])
    executeMany conn "INSERT INTO post_minor_photos (post_id, image_id) \
        \VALUES (?,?)" postPhotos
    let postTags = map (\x -> (postId draft, x)) $ tagId draft
    executeMany conn "INSERT INTO post_tags (post_id, tag_id) VALUES (?,?)" postTags
    [Only mainPhotoId] <- query conn "SELECT image_id FROM drafts WHERE draft_id = ?" (Only $ draftId draft)
    execute conn "UPDATE posts SET category_id = ?, \
        \name = ?, text = ?, image_id = ? WHERE post_id = ?" 
        (categoryId draft, name draft, description draft, mainPhotoId :: Integer, postId draft)
    return ()

hasPost :: DraftId -> Connection -> IO Bool
hasPost draftId conn = do
    [Only n] <- query conn "SELECT COUNT(post_id) FROM drafts WHERE draft_id = ?" (Only draftId)
    return $ (n :: Integer) == 1

doesExist :: DraftId -> Connection -> IO (Either Text ())
doesExist draftId conn = do 
    [Only n] <- query conn "SELECT COUNT(draft_id) FROM drafts WHERE draft_id = ?" (Only draftId)
    if (n :: Integer) == 1
    then return $ Right ()
    else return $ Left draftNotExist