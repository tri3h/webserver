{-# LANGUAGE OverloadedStrings #-}
module Utility where

import Data.Text (Text, append, unpack, splitOn)
import Types.Image ( Image(..) )
import Network.HTTP.Types.URI ( QueryText )
import Control.Monad ( join )
import Data.Text(unpack)

getText :: QueryText -> Text -> Either Text Text
getText query name = case join $ lookup name query of
    Nothing -> Left $ "No " `append` name `append` " specified"
    Just x -> Right x

getInteger :: QueryText -> Text -> Either Text Integer
getInteger query name = case join $ lookup name query of
    Nothing -> Left $ "No " `append` name `append` " specified"
    Just x -> Right (read $ unpack x :: Integer)

getIntegers :: QueryText -> Text -> Either Text [Integer]
getIntegers query name = case join $ lookup name query of
    Nothing -> Left $ "No " `append` name `append` " specified"
    Just xs -> Right $ map (\x -> read $ unpack x :: Integer) (splitOn "," xs)

getImage :: QueryText -> Text -> Either Text Image
getImage query name = case join $ lookup name query of
    Nothing -> Left $ "No " `append` name `append` " specified"
    Just x -> Right $ Image x

getImages :: QueryText -> Text -> Either Text [Image]
getImages query name = case join $ lookup name query of
    Nothing -> Left $ "No " `append` name `append` " specified"
    Just xs -> Right $ map Image (splitOn "," xs)

getMaybeText :: QueryText -> Text -> Maybe Text
getMaybeText query name = join $ lookup name query

getMaybeInteger :: QueryText -> Text -> Maybe Integer
getMaybeInteger query name = fmap (\x -> read $ unpack x :: Integer) (join $ lookup name query)

getMaybeIntegers :: QueryText -> Text -> Maybe [Integer]
getMaybeIntegers query name = fmap (map (\x -> read $ unpack x :: Integer) . splitOn ",") (join $ lookup name query)

getMaybeImage :: QueryText -> Text -> Maybe Image
getMaybeImage query name = fmap Image (join $ lookup name query)

getMaybeImages :: QueryText -> Text -> Maybe [Image]
getMaybeImages query name = fmap (map Image . splitOn ",") (join $ lookup name query)

getBool :: QueryText -> Text -> Bool
getBool query name = case join $ lookup name query of
    Nothing -> False 
    Just x -> read $ unpack x :: Bool