module Lib where

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair l r) = Pair (f l) (f r)


data IncrementPair a b =
  IncrementPair a b deriving (Eq, Show)

instance Functor (IncrementPair a) where
  fmap f (IncrementPair l r) = IncrementPair l (f r)


data BackwardPair a = BackwardPair a a deriving (Show, Eq)

instance Functor BackwardPair where
  fmap f (BackwardPair l r) = BackwardPair (f r) (f l)
