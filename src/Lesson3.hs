{-# LANGUAGE InstanceSigs #-}

module Lesson3 where

import FunctorLesson (Pair(..))

data FlippedEither b a = Error a | Success b
                       deriving Show


instance Functor (FlippedEither s) where
  fmap :: (a -> b) -> FlippedEither s a -> FlippedEither s b
  fmap f (Error x) = Error (f x)
  fmap _ (Success x) = Success x


data Twople b a = Twople a b deriving Show


instance Functor (Twople s) where
  fmap :: (a -> b) -> Twople s a -> Twople s b
  fmap f (Twople x y) = Twople (f x) y


composed :: Functor f => f [a] -> f [a]
composed = fmap (reverse . take 5)

composedfmap :: Functor f => f [a] -> f [a]
composedfmap = fmap reverse . fmap (take 5)


test :: IO ()
test =
  do
    print (fmap id (Pair "franco" "sosa"))
    print (id (Pair "franco" "sosa"))
    print (composed (Pair "franco" "sosa"))
    print (composedfmap (Pair "franco" "sosa"))


data IncrementPair a =
  IncrementPair Integer a deriving (Show, Eq)

instance Functor IncrementPair where
  fmap f (IncrementPair int r) = IncrementPair (int + 1) (f r)


testIncrementPair :: IO ()
testIncrementPair =
  do
    print (fmap id (IncrementPair 0 "123456789"))
    print (id (IncrementPair 0 "123456789"))
    print (composed (IncrementPair 0 "123456789"))
    print (composedfmap (IncrementPair 0 "123456789"))
    
