{-# LANGUAGE OverloadedStrings #-}

module Error where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (ToJSON (toJSON))
import Data.Text (Text, append, unpack)

data ErrorType = UserErr | AuthorErr | TagErr | CategoryErr | DraftErr | PostErr | CommentErr | ImageErr | OtherErr deriving (Show)

newtype ErrorNum = ErrorNum Integer deriving (Show)

newtype ErrorMes = ErrorMes Text deriving (Show)

type Name = Text

data Error = Error
  { errorType :: ErrorType,
    errorNum :: ErrorNum,
    errorMes :: ErrorMes
  }
  deriving (Show)

instance ToJSON Error where
  toJSON (Error _ (ErrorNum n) (ErrorMes m)) = toJSON $ "Error " ++ show n ++ ". " ++ unpack m

unknownError :: Error
unknownError = Error OtherErr (ErrorNum 1) (ErrorMes "Unknown error")

userNotExist :: Error
userNotExist = Error UserErr (ErrorNum 2) (ErrorMes "User with such id doesn't exist")

loginTaken :: Error
loginTaken = Error UserErr (ErrorNum 3) (ErrorMes "Login is already taken")

invalidData :: Error
invalidData = Error UserErr (ErrorNum 4) (ErrorMes "Invalid data")

tagNotExist :: Error
tagNotExist = Error TagErr (ErrorNum 5) (ErrorMes "Tag with such id doesn't exist")

tagNameTaken :: Error
tagNameTaken = Error TagErr (ErrorNum 6) (ErrorMes "Tag name is already taken")

postNotExist :: Error
postNotExist = Error PostErr (ErrorNum 7) (ErrorMes "Post with such id doesn't exist")

noPost :: Error
noPost = Error PostErr (ErrorNum 8) (ErrorMes "No posts with such parameters")

imageNotExist :: Error
imageNotExist = Error ImageErr (ErrorNum 9) (ErrorMes "Image with such id doesn't exist")

draftNotExist :: Error
draftNotExist = Error DraftErr (ErrorNum 10) (ErrorMes "Draft with such id doesn't exist")

noDeleteHasPost :: Error
noDeleteHasPost = Error DraftErr (ErrorNum 11) (ErrorMes "Impossible to delete: has post")

noDraftAuthor :: Error
noDraftAuthor = Error DraftErr (ErrorNum 12) (ErrorMes "This author isn't author of the draft")

userNotAuthor :: Error
userNotAuthor = Error AuthorErr (ErrorNum 13) (ErrorMes "The user isn't author")

commentNotExist :: Error
commentNotExist = Error CommentErr (ErrorNum 14) (ErrorMes "Comment with such id doesn't exist")

categoryNotExist :: Error
categoryNotExist = Error CategoryErr (ErrorNum 15) (ErrorMes "Category with such id doesn't exist")

invalidParent :: Error
invalidParent = Error CategoryErr (ErrorNum 16) (ErrorMes "Invalid parent id")

noDeleteHasChildren :: Error
noDeleteHasChildren = Error CategoryErr (ErrorNum 17) (ErrorMes "Impossible to delete: the category has children")

categoryNameTaken :: Error
categoryNameTaken = Error CategoryErr (ErrorNum 18) (ErrorMes "Category name is already taken")

authorNotExist :: Error
authorNotExist = Error AuthorErr (ErrorNum 19) (ErrorMes "Author with such id doesn't exist")

alreadyAuthor :: Error
alreadyAuthor = Error AuthorErr (ErrorNum 20) (ErrorMes "This user is already author")

noImage :: Error
noImage = Error ImageErr (ErrorNum 21) (ErrorMes "No image with such name")

noSpecified :: Name -> Error
noSpecified name = Error OtherErr (ErrorNum 22) (ErrorMes $ "No " `append` name `append` " specified")
