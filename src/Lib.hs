{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Lib where


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


composed :: Functor f => f [a] -> f [a]
composed = fmap (reverse . take 5)

composedfmap :: Functor f => f [a] -> f [a]
composedfmap = fmap reverse . fmap (take 5)


data Username a = Username a a deriving Show


instance Functor Username where
  fmap f (Username x y) = Username (f x) (f y)


user :: Username String
user = Username "Julie " "Moronuki"

users :: [Username String]
users = [user, Username "Chris " "Martin"]


data FlippedEither b a = Error a | Success b
  deriving Show

instance Functor (FlippedEither s) where
  fmap :: (a -> b) -> FlippedEither s a -> FlippedEither s b
  fmap f (Error x) = Error (f x)
  fmap _ (Success y) = Success y


data FlippedPair b a = MkFlippedPair a b
  deriving Show

instance Functor (FlippedPair s) where
  fmap :: (a -> b) -> FlippedPair s a -> FlippedPair s b
  fmap f (MkFlippedPair x y) = MkFlippedPair (f x) y


data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair l r) = Pair (f l) (f r)


data IncrementPair a = IncrementPair Integer a
  deriving (Show, Eq)

instance Functor IncrementPair where
  fmap f (IncrementPair int x) = IncrementPair (int + 1) (f x)


data BackwardPair a = BackwardPair a a
  deriving (Show, Eq)

instance Functor BackwardPair where
  fmap f (BackwardPair l r) = BackwardPair (f r) (f l)


composedNum :: (Functor f, Num a) => f a -> f a
composedNum = fmap (abs . (subtract 100))

fcomposedNum :: (Functor f, Num a) => f a -> f a
fcomposedNum = fmap abs . fmap (subtract 100)
