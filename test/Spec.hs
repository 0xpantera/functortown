{-# LANGUAGE OverloadedStrings #-}

import Lib

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


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


inc_id :: Property
inc_id =
  property $ do
    x  <- forAll $ Gen.integral (Range.linear 0 100)
    ys <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    fmap id (IncrementPair x ys) === id (IncrementPair x ys)


inc_comp :: Property
inc_comp =
  property $ do
    x <- forAll $ Gen.integral (Range.linear 0 100)
    y <- forAll $ Gen.integral (Range.linear 0 100)
    composedNum (IncrementPair x y) === fcomposedNum (IncrementPair x y)
    

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
        ("inc_id", inc_id),
        ("inc_comp", inc_comp)
      ]
    return ()
