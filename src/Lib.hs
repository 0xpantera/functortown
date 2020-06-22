{-# LANGUAGE InstanceSigs #-}
module Lib where

import Data.Bifunctor
-- import Data.Align


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
  deriving (Eq, Show)

instance Functor (These a) where
  fmap _ (This x) = This x
  fmap f (That y) = That (f y)
  fmap f (These x y) = These x (f y)

instance Bifunctor These where
  bimap :: (a -> b) -> (c -> d) -> (These a c) -> (These b d)
  bimap f _ (This x) = This (f x)
  bimap _ g (That y) = That (g y)
  bimap f g (These x y) = These (f x) (g y)

  first :: (a -> b) -> These a c -> These b c
  first f (This x) = This (f x)
  first _ (That y) = That y
  first f (These x y) = These (f x) y
  second :: (c -> d) -> These a c -> These a d
  
  second _ (This x) = This x
  second g (That y) = That (g y)
  second g (These x y) = These x (g y)

{-|
instance Monoid a => Align (Either a) where
  nil = Left mempty
  align :: Either a b -> Either a c -> Either a (These b c)
  align (Left x) (Left x') = Left (x <> x')
  align (Right y) (Left _) = Right (This y)
  align (Left _) (Right y) = Right (That y)
  align (Right y) (Right y') = Right (These y y')
-}

data Pair a = Zero | Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair l r) = Pair (f l) (f r)
  fmap _ Zero = Zero

{-|
instance Align Pair where
  nil = Zero
  align Zero Zero = Zero
  align (Pair x y) Zero = Pair (This x) (This y)
  align Zero (Pair x y) = Pair (That x) (That y)
  align (Pair x y) (Pair x' y') = Pair (These x x') (These y y')
-}