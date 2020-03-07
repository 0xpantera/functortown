{-# LANGUAGE InstanceSigs #-}

module Lesson3 where


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
