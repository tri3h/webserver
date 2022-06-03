{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Data.Functor.Identity (Identity)
import qualified Handlers.Author as Author
import qualified Handlers.Category as Category
import qualified Handlers.Comment as Comment
import qualified Handlers.Draft as Draft
import qualified Handlers.Image as Image
import qualified Handlers.Logger as Logger
import qualified Handlers.Post as Post
import qualified Handlers.Server as Server
import qualified Handlers.Tag as Tag
import qualified Handlers.User as User
import qualified Types.Author as T.Author
import qualified Types.Category as T.Category
import qualified Types.Draft as T.Draft
import qualified Types.Image as T.Image
import qualified Types.Post as T.Post
import Types.PostComment (PostId (PostId))
import qualified Types.Tag as T.Tag
import qualified Types.User as T.User

serverHandle :: Server.Handle Identity
serverHandle =
  Server.Handle
    { Server.hUser = userHandle,
      Server.hAuthor = authorHandle,
      Server.hCategory = categoryHandle,
      Server.hComment = commentHandle,
      Server.hDraft = draftHandle,
      Server.hImage = imageHandle,
      Server.hPost = postHandle,
      Server.hTag = tagHandle
    }

userHandle :: User.Handle Identity
userHandle =
  User.Handle
    { User.hCreate = return . Right . T.User.fToken,
      User.hGet = \token f -> return getUser,
      User.hGetRandomNumber = return 1,
      User.hIsLoginValid = \_ -> return True,
      User.hIsTokenUnique = \_ -> return True,
      User.hFindPassword = \_ -> return . User.hashPassword $ T.User.Password "password",
      User.hUpdateToken = \_ _ -> return (),
      User.hDelete = \_ -> return $ Right (),
      User.hAddAvatar = \_ _ -> return (),
      User.hIsAdmin = \_ -> return True,
      User.hIsTokenValid = \_ -> return True
    }

getUser :: T.User.GetUser
getUser =
  T.User.GetUser
    { T.User.gUserId = T.User.UserId 1,
      T.User.gName = T.User.Name "name",
      T.User.gSurname = T.User.Surname "surname",
      T.User.gAvatar = Nothing,
      T.User.gLogin = T.User.Login "login",
      T.User.gDate = T.User.Date "01-01-2000"
    }

authorHandle :: Author.Handle Identity
authorHandle =
  Author.Handle
    { Author.hCreate = \_ -> return $ Right (),
      Author.hGet = \_ -> return $ Right getAuthor,
      Author.hEdit = \_ -> return $ Right (),
      Author.hDelete = \_ -> return $ Right ()
    }

getAuthor :: T.Author.GetAuthor
getAuthor =
  T.Author.GetAuthor
    { T.Author.gAuthorId = T.Author.AuthorId 1,
      T.Author.gUserId = Nothing,
      T.Author.gDescription = T.Author.Description "description"
    }

categoryHandle :: Category.Handle Identity
categoryHandle =
  Category.Handle
    { Category.hDelete = \_ -> return $ Right (),
      Category.hEditName = \_ _ -> return $ Right (),
      Category.hEditParent = \_ _ -> return $ Right (),
      Category.hGetChildren = \_ -> return [],
      Category.hCreate = \_ -> return $ Right (),
      Category.hGet = \_ -> return $ Right getCategory,
      Category.hGetAll = \_ _ -> return []
    }

getCategory :: T.Category.GetCategory
getCategory =
  T.Category.GetCategory
    { T.Category.gCategoryId = T.Category.CategoryId 1,
      T.Category.gName = T.Category.Name "name",
      T.Category.gParentId = Nothing
    }

commentHandle :: Comment.Handle Identity
commentHandle =
  Comment.Handle
    { Comment.hCreate = \_ -> return $ Right (),
      Comment.hGetEither = \_ _ _ -> return $ Right [],
      Comment.hDelete = \_ -> return $ Right ()
    }

draftHandle :: Draft.Handle Identity
draftHandle =
  Draft.Handle
    { Draft.hEditCategoryId = \_ _ -> return $ Right (),
      Draft.hEditTagId = \_ _ -> return $ Right (),
      Draft.hEditName = \_ _ -> return $ Right (),
      Draft.hEditDescription = \_ _ -> return $ Right (),
      Draft.hEditMainPhoto = \_ _ -> return $ Right (),
      Draft.hDelete = \_ -> return $ Right (),
      Draft.hAddMinorPhoto = \_ _ -> return $ Right (),
      Draft.hDeleteMinorPhoto = \_ _ -> return $ Right (),
      Draft.hHasPost = \_ -> return True,
      Draft.hGet = \_ _ -> return getDraft,
      Draft.hGetAuthorIdByDraftId = \_ -> return . Right $ T.Author.AuthorId 1,
      Draft.hPublish = \_ -> return $ Right (),
      Draft.hUpdate = \_ -> return $ Right (),
      Draft.hGetAuthorIdByToken = \_ -> return . Right $ T.Author.AuthorId 1,
      Draft.hCreate = \_ -> return . Right $ T.Draft.DraftId 1,
      Draft.hGetAllByAuthor = \_ _ _ -> return []
    }

getDraft :: T.Draft.GetDraft
getDraft =
  T.Draft.GetDraft
    { T.Draft.gDraftId = T.Draft.DraftId 1,
      T.Draft.gPostId = Nothing,
      T.Draft.gAuthorId = Nothing,
      T.Draft.gCategoryId = Nothing,
      T.Draft.gTagId = [],
      T.Draft.gName = T.Draft.Name "name",
      T.Draft.gText = "text",
      T.Draft.gMainPhoto = Nothing,
      T.Draft.gMinorPhoto = []
    }

imageHandle :: Image.Handle Identity
imageHandle =
  Image.Handle
    { Image.hGet = \_ -> return . Right $ T.Image.Image "" ""
    }

postHandle :: Post.Handle Identity
postHandle =
  Post.Handle
    { Post.hGet = \_ _ _ _ _ -> return [shortPost],
      Post.hGetMinorPhotos = \_ _ -> return [],
      Post.hGetAuthor = \_ -> return Nothing,
      Post.hGetUser = \_ -> return Nothing,
      Post.hGetCategory = \_ -> return [],
      Post.hGetTag = \_ -> return [],
      Post.hGetComment = \_ -> return []
    }

shortPost :: T.Post.ShortPost
shortPost =
  T.Post.ShortPost
    { T.Post.sPostId = PostId 1,
      T.Post.sCategoryId = Nothing,
      T.Post.sAuthorId = Nothing,
      T.Post.sName = T.Post.Name "name",
      T.Post.sDate = T.Post.Date "10-10-2020",
      T.Post.sText = "text",
      T.Post.sMainPhoto = Nothing
    }

fullPost :: T.Post.FullPost
fullPost =
  T.Post.FullPost
    { T.Post.fPostId = PostId 1,
      T.Post.fUser = Nothing,
      T.Post.fAuthor = Nothing,
      T.Post.fCategory = [],
      T.Post.fTag = [],
      T.Post.fComment = [],
      T.Post.fName = T.Post.Name "name",
      T.Post.fDate = T.Post.Date "10-10-2020",
      T.Post.fText = "text",
      T.Post.fMainPhoto = Nothing,
      T.Post.fMinorPhoto = []
    }

tagHandle :: Tag.Handle Identity
tagHandle =
  Tag.Handle
    { Tag.hCreate = \_ -> return $ Right (),
      Tag.hGet = \_ -> return $ Right tag,
      Tag.hGetAll = \_ _ -> return [],
      Tag.hEdit = \_ -> return $ Right (),
      Tag.hDelete = \_ -> return $ Right ()
    }

tag :: T.Tag.Tag
tag =
  T.Tag.Tag
    { T.Tag.tagId = T.Tag.TagId 1,
      T.Tag.name = T.Tag.Name "name"
    }

loggerHandle :: Logger.Handle Identity
loggerHandle =
  Logger.Handle
    { Logger.hWriteLog = \_ -> return (),
      Logger.hVerbosity = Logger.Info
    }
