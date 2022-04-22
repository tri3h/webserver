{-# LANGUAGE OverloadedStrings #-}

module Utility where

import Control.Monad (join)
import Data.Text (Text, append, isInfixOf, null, pack, replace, splitOn, unpack)
import Network.HTTP.Types.URI (QueryText)
import Text.Read (readMaybe)
import Types.Config (ServerAddress)
import Types.Image (Image (Id, Link), imageAddress, malformedImage)
import Prelude hiding (null)

getText :: QueryText -> Text -> Either Text Text
getText query name = case join $ lookup name query of
  Nothing -> noSpecified name
  Just x ->
    if null x
      then noSpecified name
      else Right x

getInteger :: QueryText -> Text -> Either Text Integer
getInteger query name = case join $ lookup name query of
  Nothing -> noSpecified name
  Just x -> maybe (noSpecified name) Right (readMaybe $ unpack x :: Maybe Integer)

getIntegers :: QueryText -> Text -> Either Text [Integer]
getIntegers query name = case join $ lookup name query of
  Nothing -> noSpecified name
  Just xs ->
    let values = map (\x -> readMaybe $ unpack x :: Maybe Integer) (splitOn "," xs)
     in if Nothing `elem` values
          then noSpecified name
          else Right $ map (\(Just x) -> x) values

getImage :: Text -> Text -> Either Text Text
getImage body name =
  let hasImage = ("name=\"" `append` name) `isInfixOf` body
   in if hasImage
        then
          Right $
            replace "\n" "" $
              head $
                tail $
                  dropWhile (/= "") $ splitOn "\r\n" body
        else Left noImage

getMaybeText :: QueryText -> Text -> Maybe Text
getMaybeText query name = case join $ lookup name query of
  Nothing -> Nothing
  Just x -> if null x then Nothing else Just x

getMaybeInteger :: QueryText -> Text -> Maybe Integer
getMaybeInteger query name = case join $ lookup name query of
  Nothing -> Nothing
  Just x -> readMaybe $ unpack x

getMaybeIntegers :: QueryText -> Text -> Maybe [Integer]
getMaybeIntegers query name = case join $ lookup name query of
  Nothing -> Nothing
  Just xs -> mapM (\x -> readMaybe $ unpack x :: Maybe Integer) (splitOn "," xs)

getMaybeImage :: Text -> Text -> Maybe Text
getMaybeImage body name =
  let hasImage = ("name=\"" `append` name) `isInfixOf` body
   in if hasImage
        then
          Just $
            replace "\n" "" $
              head $
                tail $
                  dropWhile (/= "") $ splitOn "\r\n" body
        else Nothing

imageToLink :: ServerAddress -> Image -> Either Text Image
imageToLink server (Id x) = Right . Link $ server `append` imageAddress `append` pack (show x)
imageToLink _ link@(Link _) = Right link
imageToLink _ _ = Left malformedImage

imagesToLinks :: ServerAddress -> [Image] -> Either Text [Image]
imagesToLinks server = mapM (imageToLink server)

noSpecified :: Text -> Either Text b
noSpecified name = Left $ "No " `append` name `append` " specified"

noImage :: Text
noImage = "No image with such name"
