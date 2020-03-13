module Lib where

import Data.Bifunctor

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair l r) = Pair (f l) (f r)


data BackwardPair a = BackwardPair a a deriving (Show, Eq)

instance Functor BackwardPair where
  fmap f (BackwardPair l r) = BackwardPair (f r) (f l)


data IncrementPair a b =
  IncrementPair a b deriving (Eq, Show)

instance Functor (IncrementPair a) where
  fmap f (IncrementPair l r) = IncrementPair l (f r)

instance Bifunctor IncrementPair where
  bimap f g (IncrementPair l r) =
    IncrementPair (f l) (g r)


data These a b = This a | That b | These a b
  deriving Show

instance Functor (These a) where
  fmap _ (This x) = This x
  fmap f (That y) = That (f y)
  fmap f (These x y) = These x (f y)

instance Bifunctor These where
  bimap f _ (This x) = This (f x)
  bimap _ g (That y) = That (g y)
  bimap f g (These x y) = These (f x) (g y)
