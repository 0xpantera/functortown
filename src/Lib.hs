{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Lib where

import Data.Align
import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.These as TH


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


data Pair a = Zero | Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f Zero = Zero
  fmap f (Pair l r) = Pair (f l) (f r)

instance Align Pair where
  nil = Zero
  align Zero Zero = Zero
  align (Pair x y) Zero = Pair (TH.This x) (TH.This y)
  align Zero (Pair x y) = Pair (TH.That x) (TH.That y)
  align (Pair x y) (Pair x' y') = Pair (TH.These x x') (TH.These y y')

data IncrementPair a b = IncrementPair a b
  deriving (Show, Eq)

instance Functor (IncrementPair a) where
  fmap f (IncrementPair l r) = IncrementPair l (f r)

instance Bifunctor IncrementPair where
  bimap f g (IncrementPair int r) = IncrementPair (f int) (g r)
  
increMap :: (Num a) => (b -> c) -> IncrementPair a b -> IncrementPair a c
increMap = bimap (+1)

data BackwardPair a = BackwardPair a a
  deriving (Show, Eq)

instance Functor BackwardPair where
  fmap f (BackwardPair l r) = BackwardPair (f r) (f l)


data These a b = This a | That b | These a b
  deriving (Eq, Show)

instance Functor (These a) where
  fmap f (This a) = This a
  fmap f (That r) = That (f r)
  fmap f (These l r) = These l (f r)

instance Bifunctor These  where
  bimap f _ (This l) = This (f l)
  bimap _ g (That r) = That (g r)
  bimap f g (These l r) = These (f l) (g r)


instance Monoid a => Align (Either a) where
  nil = Left mempty
  align :: Either a b -> Either a c -> Either a (TH.These b c)
  align (Left x) (Left x') = Left x
  align (Left x) (Right y) = Right (TH.That y)
  align (Right y) (Left x) = Right (TH.This y)
  align (Right y) (Right z) = Right (TH.These y z)
  
  
composedNum :: (Functor f, Num a) => f a -> f a
composedNum = fmap (abs . (subtract 100))

fcomposedNum :: (Functor f, Num a) => f a -> f a
fcomposedNum = fmap abs . fmap (subtract 100)
