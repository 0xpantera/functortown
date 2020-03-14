{-# LANGUAGE InstanceSigs #-}
module Lib where

import Data.Bifunctor
import Data.Align
import qualified Data.These as TH


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


data These' a b = This' a | That' b | These' a b
  deriving Show

instance Functor (These' a) where
  fmap _ (This' x) = This' x
  fmap f (That' y) = That' (f y)
  fmap f (These' x y) = These' x (f y)

instance Bifunctor These' where
  bimap f _ (This' x) = This' (f x)
  bimap _ g (That' y) = That' (g y)
  bimap f g (These' x y) = These' (f x) (g y)


instance Monoid a => Align (Either a) where
  nil = Left mempty
  align :: Either a b -> Either a c -> Either a (TH.These b c)
  align (Left x) (Left x') = Left (x <> x')
  align (Right y) (Left _) = Right (TH.This y)
  align (Left _) (Right y) = Right (TH.That y)
  align (Right y) (Right y') = Right (TH.These y y')


data Pair a = Zero | Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair l r) = Pair (f l) (f r)
  fmap _ Zero = Zero

instance Align Pair where
  nil = Zero
  align Zero Zero = Zero
  align (Pair x y) Zero = Pair (TH.This x) (TH.This y)
  align Zero (Pair x y) = Pair (TH.That x) (TH.That y)
  align (Pair x y) (Pair x' y') = Pair (TH.These x x') (TH.These y y')
