{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where


import qualified Data.Text as T


someFunc :: IO ()
someFunc = putStrLn "someFunc"


database :: [(Integer, String)]
database = [ (1, "Julie")
           , (2, "Chris")
           , (3, "Alonzo")
           , (4, "Melman") ]


greetUser :: Integer -> Maybe T.Text
greetUser record =
  mapToMaybe ("Hello, " <>) (lookup record newDatabase)


mapToMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapToMaybe function Nothing = Nothing
mapToMaybe function (Just x) = Just (function x)


mapToEither :: (a -> b) -> (Either e a -> Either e b)
mapToEither f (Left err) = Left err
mapToEither f (Right x) = Right (f x)


convertToText :: (Integer, String) -> (Integer, T.Text)
convertToText xs = fmap T.pack xs


convertDatabase :: [(Integer, String)] -> [(Integer, T.Text)]
convertDatabase xs = fmap convertToText xs


cleanupDatabase :: [(Integer, String)] -> [(Integer, T.Text)]
cleanupDatabase xs = (fmap . fmap) T.strip (convertDatabase xs)


newDatabase :: [(Integer, T.Text)]
newDatabase = cleanupDatabase database


