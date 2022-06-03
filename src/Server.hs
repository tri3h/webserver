{-# LANGUAGE OverloadedStrings #-}

module Server where

import qualified Admin
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, withResource)
import Data.String (IsString (fromString))
import Data.Text (unpack)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Author as Db.Author
import qualified Database.Queries.Category as Db.Category
import qualified Database.Queries.Comment as Db.Comment
import qualified Database.Queries.Draft as Db.Draft
import qualified Database.Queries.Image as Db.Image
import qualified Database.Queries.Post as Db.Post
import qualified Database.Queries.Tag as Db.Tag
import qualified Database.Queries.User as Db.User
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
import Network.HTTP.Types (queryToQueryText)
import Network.Wai
  ( Request (queryString),
    strictRequestBody,
  )
import qualified Network.Wai.Handler.Warp as Warp
import System.Random (randomIO)
import Types.Author (AuthorId (AuthorId))
import Types.Config (Config (server), ServerConfig (sAddress, sHost, sPort))
import qualified Types.Config as Config
import Types.User (UserId (UserId))

run :: Logger.Handle IO -> Config.Config -> Pool Connection -> IO ()
run logger config pool = do
  let port = fromInteger . sPort $ server config
  let host = fromString . unpack . sHost $ server config
  let addr = sAddress $ server config
  let onExp _ e = Logger.error logger $ show e
  let settings = Warp.setOnException onExp $ Warp.setHost host $ Warp.setPort port Warp.defaultSettings
  Admin.check (userHandle pool) logger pool
  Logger.info logger "Starting server"
  Warp.runSettings settings $ \req f -> do
    let query = queryToQueryText $ queryString req
    Logger.debug logger $ "Received query: " ++ show query
    body <- toStrict <$> strictRequestBody req
    Logger.debug logger $ "Received body: " ++ show body
    response <- Server.makeNoTokenResponse (serverHandle pool) logger addr req body
    f response

serverHandle :: Pool Connection -> Server.Handle IO
serverHandle pool =
  Server.Handle
    { Server.hUser = userHandle pool,
      Server.hAuthor = authorHandle pool,
      Server.hCategory = categoryHandle pool,
      Server.hComment = commentHandle pool,
      Server.hDraft = draftHandle pool,
      Server.hImage = imageHandle pool,
      Server.hPost = postHandle pool,
      Server.hTag = tagHandle pool
    }

userHandle :: Pool Connection -> User.Handle IO
userHandle pool =
  let f = withResource pool
   in User.Handle
        { User.hIsLoginValid = f . Db.User.isLoginValid,
          User.hIsTokenUnique = f . Db.User.isTokenUnique,
          User.hCreate = f . Db.User.create,
          User.hGet = \a b -> f $ Db.User.get a b,
          User.hGetRandomNumber = randomIO,
          User.hFindPassword = f . Db.User.findPassword,
          User.hUpdateToken = \a b -> f $ Db.User.updateToken a b,
          User.hDelete = f . Db.User.delete,
          User.hAddAvatar = \a b -> f $ Db.User.addAvatar a b,
          User.hIsAdmin = f . Db.User.isAdmin,
          User.hIsTokenValid = f . Db.User.isTokenValid
        }

tagHandle :: Pool Connection -> Tag.Handle IO
tagHandle pool =
  let f = withResource pool
   in Tag.Handle
        { Tag.hCreate = f . Db.Tag.create,
          Tag.hGet = f . Db.Tag.get,
          Tag.hGetAll = \a b -> f $ Db.Tag.getAll a b,
          Tag.hEdit = f . Db.Tag.edit,
          Tag.hDelete = f . Db.Tag.delete
        }

postHandle :: Pool Connection -> Post.Handle IO
postHandle pool =
  let f = withResource pool
   in Post.Handle
        { Post.hGet = \a b c d e -> f $ Db.Post.get a b c d e,
          Post.hGetMinorPhotos = \a b -> f $ Db.Post.getMinorPhotos a b,
          Post.hGetAuthor = f . Db.Author.getMaybe . fromMaybe (AuthorId 0),
          Post.hGetUser = f . Db.User.getPostUser . fromMaybe (UserId 0),
          Post.hGetCategory = f . Db.Category.getWithParents,
          Post.hGetTag = f . Db.Tag.getByPostId,
          Post.hGetComment = f . Db.Comment.get
        }

imageHandle :: Pool Connection -> Image.Handle IO
imageHandle pool =
  Image.Handle
    { Image.hGet = withResource pool . Db.Image.get
    }

draftHandle :: Pool Connection -> Draft.Handle IO
draftHandle pool =
  let f = withResource pool
   in Draft.Handle
        { Draft.hEditCategoryId = \a b -> f $ Db.Draft.editCategoryId a b,
          Draft.hEditTagId = \a b -> f $ Db.Draft.editTagId a b,
          Draft.hEditName = \a b -> f $ Db.Draft.editName a b,
          Draft.hEditDescription = \a b -> f $ Db.Draft.editText a b,
          Draft.hEditMainPhoto = \a b -> f $ Db.Draft.editMainPhoto a b,
          Draft.hHasPost = f . Db.Draft.hasPost,
          Draft.hDelete = f . Db.Draft.delete,
          Draft.hGet = \a b -> f $ Db.Draft.get a b,
          Draft.hGetAuthorIdByDraftId = f . Db.Author.getByDraftId,
          Draft.hPublish = f . Db.Draft.publish,
          Draft.hUpdate = f . Db.Draft.update,
          Draft.hGetAuthorIdByToken = f . Db.Author.getByToken,
          Draft.hAddMinorPhoto = \a b -> f $ Db.Draft.addMinorPhoto a b,
          Draft.hDeleteMinorPhoto = \a b -> f $ Db.Draft.deleteMinorPhoto a b,
          Draft.hCreate = f . Db.Draft.create,
          Draft.hGetAllByAuthor = \a b c -> f $ Db.Draft.getAllByAuthor a b c
        }

commentHandle :: Pool Connection -> Comment.Handle IO
commentHandle pool =
  let f = withResource pool
   in Comment.Handle
        { Comment.hCreate = f . Db.Comment.create,
          Comment.hGetEither = \a b c -> f $ Db.Comment.getEither a b c,
          Comment.hDelete = f . Db.Comment.delete
        }

categoryHandle :: Pool Connection -> Category.Handle IO
categoryHandle pool =
  let f = withResource pool
   in Category.Handle
        { Category.hDelete = f . Db.Category.delete,
          Category.hEditName = \a b -> f $ Db.Category.editName a b,
          Category.hEditParent = \a b -> f $ Db.Category.editParent a b,
          Category.hGetChildren = f . Db.Category.getChildren,
          Category.hCreate = f . Db.Category.create,
          Category.hGet = f . Db.Category.get,
          Category.hGetAll = \a b -> f $ Db.Category.getAll a b
        }

authorHandle :: Pool Connection -> Author.Handle IO
authorHandle pool =
  let f = withResource pool
   in Author.Handle
        { Author.hCreate = f . Db.Author.create,
          Author.hGet = f . Db.Author.get,
          Author.hEdit = f . Db.Author.edit,
          Author.hDelete = f . Db.Author.delete
        }
