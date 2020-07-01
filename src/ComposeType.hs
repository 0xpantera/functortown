{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor, DerivingVia #-}
module ComposeType where

import Control.Applicative


newtype ReaderIO env a = ReaderIO (env -> IO a)

instance Functor (ReaderIO env) where
    fmap :: (a -> b) -> ReaderIO env a -> ReaderIO env b
    fmap f (ReaderIO g) = ReaderIO $ fmap (fmap f) g

instance Applicative (ReaderIO env) where
    pure :: a -> ReaderIO env a
    pure x = ReaderIO $ composePure x

    liftA2 :: (a -> b -> c) -> ReaderIO env a -> ReaderIO env b -> ReaderIO env c
    liftA2 f (ReaderIO g) (ReaderIO h) = ReaderIO $ composeLiftA2 f g h


composePure ::
    (Applicative f1, Applicative f2) =>
    a -> f1 (f2 a)
composePure x = pure (pure x)


composeLiftA2 ::
    (Applicative f1, Applicative f2) =>
    (a -> b -> c) -> f1 (f2 a) 
                  -> f1 (f2 b)
                  -> f1 (f2 c)
composeLiftA2 f g h = liftA2 (liftA2 f) g h


compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- Compose :: (* -> *) -> (* -> *) -> * -> *
-- The composition of two (* -> *) type constructors applied to
-- one concrete type results is one concrete type.
newtype Compose f g a =
    Compose { getCompose :: f (g a) } deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap f (Compose x) = Compose $ fmap (fmap f) x

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure x = Compose $ pure (pure x)

    liftA2 :: (a -> b -> c) -> Compose f g a -> Compose f g b -> Compose f g c
    liftA2 f (Compose g) (Compose h) = Compose $ liftA2 (liftA2 f) g h

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose x) = Compose $ liftA2 (<*>) f x


newtype MaybeList a = MaybeList (Maybe [a])
    deriving Show
    deriving Functor
    deriving Applicative via Compose Maybe []