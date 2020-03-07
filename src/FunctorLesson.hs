module FunctorLesson where
import qualified Data.Text as T

database :: [(Integer, String)]
database = [(1, "Mariela"),
            (2, "Franky"),
            (3, "Alonzo"),
            (4, "Melman")]


greetUser :: Integer -> Maybe T.Text
greetUser record =
   fmap (T.append greeting) name 
   where name = lookup record newDatabase
         greeting = T.pack "Hello "


mapToMaybe :: (a -> b) -> Maybe a -> Maybe b
mapToMaybe _ Nothing = Nothing
mapToMaybe function (Just a) = Just (function a)


mapToEither :: (a -> b) -> Either left a -> Either left b
mapToEither _ (Left l) = Left l
mapToEither f (Right r) = Right (f r)


convertToText :: (Integer, String) -> (Integer, T.Text)
convertToText xs = fmap T.pack xs


convertDatabase :: [(Integer, String)] -> [(Integer, T.Text)]
convertDatabase xs = fmap convertToText xs


cleanupDatabase :: [(Integer, String)] -> [(Integer, T.Text)]
cleanupDatabase xs = (fmap . fmap) T.strip (convertDatabase xs)


newDatabase :: [(Integer, T.Text)]
newDatabase = cleanupDatabase database


