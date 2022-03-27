{-# LANGUAGE OverloadedStrings #-}
module Database.Queries.Draft where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time (Date)
import Database.Connection
import Data.Text(Text, append, pack)
import Types.Draft
import Types.Post (PostId)
import Types.Category(CategoryId)
import Types.Tag(TagId)
import Types.Image

create :: Draft -> Connection -> IO ()
create draft conn = do
    [Only imageId] <- query conn "INSERT INTO images (image) \
    \VALUES (decode(?,'base64')) RETURNING image_id" (Only $ mainPhoto draft)
    [Only draftId] <- query conn "INSERT INTO drafts \
    \(category_id, name, text, image_id) VALUES (?,?,?,?) \
    \RETURNING draft_id" (categoryId draft, name draft, description draft, imageId :: Integer)
    let tags = map (\x -> (draftId, x)) $ tagId draft
    executeMany conn "INSERT INTO draft_tags (draft_id, tag_id) \
    \VALUES (?,?)" (tags :: [(Integer,Integer)])
    let minorPhotos = map Only $ minorPhoto draft
    mPhotoId <- returning conn "INSERT INTO images (image) VALUES \
    \(decode(?,'base64')) RETURNING image_id" minorPhotos
    let mPhotos = map (\(Only x) -> (draftId, x)) mPhotoId
    executeMany conn "INSERT INTO draft_minor_photos (draft_id, image_id) \
    \VALUES (?,?)" (mPhotos :: [(Integer,Integer)])
    return ()

get :: DraftId -> Connection -> IO Draft
get draftId conn = do
    server <- serverAddress
    [(draftId, postId, authorId, categoryId, name, description, mainPhotoId)] <-
        query conn "SELECT draft_id, post_id, author_id, category_id, name, \
        \text, image_id FROM drafts WHERE draft_id = ?" (Only draftId)
    tagIds <- query conn "SELECT tag_id FROM draft_tags WHERE draft_id = ?" (Only draftId)
    minorPhotoId <- query conn "SELECT image_id \
        \FROM draft_minor_photos WHERE draft_id = ?" (Only draftId)
    return $ Draft {
        draftId = draftId,
        postId = postId,
        authorId = authorId,
        categoryId = categoryId,
        tagId = map (\(Only x) -> x) tagIds,
        name = name,
        description = description,
        mainPhoto = Link $ server `append` "/images?image_id=" `append` pack (show (mainPhotoId :: Integer)),
        minorPhoto = map (\(Only x) -> Link $ server `append` "/images?image_id=" `append` pack (show (x :: Integer))) minorPhotoId
    }

editCategoryId :: DraftId -> CategoryId -> Connection -> IO ()
editCategoryId draftId catId conn = do
    n <- execute conn "UPDATE drafts SET category_id = ? WHERE draft_id = ?" (catId, draftId)
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
    n <- execute conn "UPDATE drafts SET name = ? WHERE draft_id = ?" (name, draftId)
    return ()

editDescription :: DraftId -> Description -> Connection -> IO ()
editDescription draftId descr conn = do
    n <- execute conn "UPDATE drafts SET text = ? WHERE draft_id = ?" (descr, draftId)
    return ()

editMainPhoto :: DraftId -> Image -> Connection -> IO ()
editMainPhoto draftId photo conn = do
    [Only imageId] <- query conn "INSERT INTO images (image) \
    \VALUES (decode(?,'base64')) RETURNING image_id" (Only photo)
    n <- execute conn "UPDATE drafts SET image_id = ? \
    \WHERE draft_id = ?" (imageId :: Integer, draftId)
    return ()

editMinorPhotos :: DraftId -> [Image] -> Connection -> IO ()
editMinorPhotos draftId photos conn = do
    let onlyPhotos = map Only photos
    imageIds <- returning conn "INSERT INTO images (image) \
    \VALUES (decode(?,'base64')) RETURNING image_id" onlyPhotos
    let draftPhotos = map (\(Only x) -> (draftId, x)) (imageIds :: [Only Integer])
    execute conn "DELETE FROM draft_minor_photos dmp INNER JOIN drafts d \
    \ON dt.draft_id = d.draft_id WHERE draft_id = ?" (Only draftId)
    executeMany conn "INSERT INTO draft_minor_photos \
    \(draft_id, minot_photo_id) VALUES (?,?)" draftPhotos
    return ()

delete :: DraftId -> Connection -> IO ()
delete draftId conn = do
    n <- execute conn "DELETE FROM drafts WHERE drafts.draft_id = ?" (Only draftId)
    return ()

publish :: Draft -> String -> Connection -> IO PostId
publish draft date conn = do
    [Only postId] <- query conn "INSERT INTO posts (author_id, category_id, \
        \name, date, text, image_id) VALUES (?,?,?,?,?,?) \
        \RETURNING post_id" (authorId draft, categoryId draft,
        name draft, date, description draft, mainPhoto draft)
    let postPhotos = map (\y -> (postId, y)) $ minorPhoto draft
    executeMany conn "INSERT INTO post_minor_photos (post_id, image_id) \
    \VALUES (?,?)" postPhotos
    let postTags = map (\x -> (postId, x)) $ tagId draft
    executeMany conn "INSERT INTO post_tags (post_id, tag_id) VALUES (?,?)" postTags
    return postId

update :: Draft -> Connection -> IO PostId
update draft conn = do
    execute conn "DELETE FROM post_tags WHERE post_id = ?" (Only $ postId draft)
    execute conn "DELETE FROM post_minor_photos WHERE post_id = ?" (Only $ postId draft)
    let postPhotos = map (\y -> (postId draft, y)) $ minorPhoto draft
    executeMany conn "INSERT INTO post_minor_photos (post_id, image_id) \
    \VALUES (?,?)" postPhotos
    let postTags = map (\x -> (postId draft, x)) $ tagId draft
    executeMany conn "INSERT INTO post_tags (post_id, tag_id) VALUES (?,?)" postTags
    [Only postId] <- query conn "UPDATE posts SET category_id = ?, \
    \name = ?, text = ?, image_id = ? WHERE post_id = ? \
    \RETURNING post_id" (categoryId draft, name draft, description draft, mainPhoto draft, postId draft)
    return postId

hasPost :: DraftId -> Connection -> IO Bool
hasPost draftId conn = do
    [Only n] <- query conn "SELECT COUNT(post_id) FROM drafts WHERE draft_id = ?" (Only draftId)
    return $ (n :: Integer) /= 0

doesExist :: DraftId -> Connection -> IO Bool
doesExist draftId conn = do 
    [Only n] <- query conn "SELECT COUNT(draft_id) FROM drafts WHERE draft_id = ?" (Only draftId)
    return $ (n :: Integer) /= 0