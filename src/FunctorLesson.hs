{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE InstanceSigs #-}

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


data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair l r) = Pair (f l) (f r)


data Username a = Username a a deriving Show

instance Functor Username where
  fmap :: (a -> b) -> Username a -> Username b
  fmap f (Username firstname lastname) = Username (f firstname) (f lastname)

userOne :: Username T.Text
userOne = Username " Franco " "  Sosa"

userTwo :: Username T.Text
userTwo = Username "Mariela " " Figueroa"


users :: [Username T.Text]
users = [userOne, userTwo]

cleanUsers :: [Username T.Text] -> [Username T.Text]
cleanUsers xs = (fmap . fmap) T.strip xs


data Triple a b c = Triple a b c deriving Show

instance Functor (Triple a b) where
  fmap :: (c -> d) -> (Triple a b c) -> (Triple a b d)
  fmap f  (Triple x y z) = Triple x y (f z)


addToTriple :: Triple a b Integer -> Integer -> Triple a b Integer
addToTriple trips adder = fmap (+ adder) trips


