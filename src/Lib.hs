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


greetUser :: Integer -> Maybe String
greetUser record =
  mapToMaybe ("Hello, " ++) (lookup record database)


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


aFunc :: [(Integer, String)] -> [(Integer, T.Text)]
aFunc xs = (fmap . fmap) T.strip (convertDatabase xs)
