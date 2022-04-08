{-# LANGUAGE OverloadedStrings #-}

module Utility where

import Control.Monad (join)
import Data.Text (Text, append, isInfixOf, null, replace, splitOn, unpack)
import Network.HTTP.Types.URI (QueryText)
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
  Just x ->
    if null x
      then noSpecified name
      else Right (read $ unpack x :: Integer)

getIntegers :: QueryText -> Text -> Either Text [Integer]
getIntegers query name = case join $ lookup name query of
  Nothing -> noSpecified name
  Just xs ->
    if null xs
      then noSpecified name
      else Right $ map (\x -> read $ unpack x :: Integer) (splitOn "," xs)

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
  Just x -> if null x then Nothing else Just (read $ unpack x :: Integer)

getMaybeIntegers :: QueryText -> Text -> Maybe [Integer]
getMaybeIntegers query name = case join $ lookup name query of
  Nothing -> Nothing
  Just xs ->
    if null xs
      then Nothing
      else Just $ map (\x -> read $ unpack x :: Integer) $ splitOn "," xs

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

noSpecified :: Text -> Either Text b
noSpecified name = Left $ "No " `append` name `append` " specified"

noImage :: Text
noImage = "No image with such name"
