{-# LANGUAGE OverloadedStrings #-}

import Lib
import Data.Bifunctor

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range



composedNum :: (Functor f, Num a) => f a -> f a
composedNum = fmap (abs . (subtract 100))

fcomposedNum :: (Functor f, Num a) => f a -> f a
fcomposedNum = fmap abs . fmap (subtract 100)


pair_id :: Property
pair_id =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    ys <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    fmap id (Pair xs ys) === id (Pair xs ys)


pair_comp :: Property
pair_comp =
  property $ do
    x <- forAll $ Gen.integral (Range.linear 0 100)
    y <- forAll $ Gen.integral (Range.linear 0 100)
    composedNum (Pair x y) === fcomposedNum (Pair x y)


backward_id :: Property
backward_id =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    ys <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    fmap id (BackwardPair xs ys) === id (BackwardPair xs ys)

backward_comp :: Property
backward_comp =
  property $ do
    x <- forAll $ Gen.integral (Range.linear 0 100)
    y <- forAll $ Gen.integral (Range.linear 0 100)
    composedNum (BackwardPair x y) === fcomposedNum (BackwardPair x y)

incPair_id :: Property
incPair_id =
  property $ do
    xs <- forAll $ Gen.integral (Range.linear 0 100)
    ys <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    fmap id (IncrementPair xs ys) === id (IncrementPair xs ys)

incPair_comp :: Property
incPair_comp =
  property $ do
    x <- forAll $ Gen.integral (Range.linear 0 100)
    y <- forAll $ Gen.integral (Range.linear 0 100)
    composedNum (IncrementPair x y) === fcomposedNum (IncrementPair x y)


tuple_id :: Property
tuple_id =
  property $ do
    x <- forAll $ Gen.element ['a' .. 'z']
    y <- forAll $ Gen.int (Range.constant minBound maxBound)
    bimap id id (x, y) === id (x, y)
    

tests :: IO ()
tests =
  do
    checkParallel $ Group "Pair Functor Tests" [
        ("pair_id", pair_id),
        ("pair_comp", pair_comp)
      ]
    checkParallel $ Group "BackwardPair Functor Tests" [
        ("backward_id", backward_id),
        ("backward_comp", backward_comp)
      ]
    checkParallel $ Group "IncrementPair Functor Tests" [
        ("incPair_id", incPair_id),
        ("incPair_comp", incPair_comp)
      ]
    return ()
