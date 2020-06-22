{-# LANGUAGE InstanceSigs #-}
module ReaderContext where

import Control.Applicative

assembleMessage :: String -> String -> String -> String
assembleMessage salutation pitch closing =
    salutation ++ "\n\n" ++ pitch ++ "\n\n" ++ closing

genericSalutation = "To whom it may concern:"

genericPitch = "I have been looking at your work and I have an \
    \opportunity in the Bay Area that seems right up your alley."

genericClosing = "- Recruiter"

genericMessage :: String
genericMessage =
    assembleMessage
        genericSalutation
        genericPitch
        genericClosing

functionPure :: a -> w -> a
functionPure = const

functionLiftA2 :: (a -> b -> c) -> (w -> a) -> (w -> b) -> w -> c
functionLiftA2 f g h x = f (g x) (h x)

functionAp :: (w -> a -> b) -> (w -> a) -> w -> b
functionAp f g x = (f x) (g x)

funcComp :: (b -> c) -> (a -> b) -> a -> c
funcComp f g x = f (g x)


newtype Reader env a =
    Reader { runReader :: env -> a }

instance Functor (Reader env) where
    fmap :: (a -> b) -> Reader env a -> Reader env b
    fmap f (Reader x) = Reader $ f . x

instance Applicative (Reader env) where
    pure :: a -> Reader env a
    pure x = Reader (const x)

    liftA2 :: (a -> b -> c) -> Reader env a -> Reader env b -> Reader env c
    liftA2 f (Reader g) (Reader h) = Reader (\env -> f (g env) (h env))

    (<*>) :: Reader env (a -> b) -> Reader env a -> Reader env b 
    (Reader f) <*> (Reader g) = Reader (\env -> f env (g env))


personalizedSalutation :: Reader String String
personalizedSalutation = Reader (\name -> "Hello " ++ name ++ ",")

personalizedPitch :: Reader String String
personalizedPitch = Reader (\name -> "I have been looking at your work, " ++ name ++
    ", and I have an opportunity in the Bay Area that seems right up your alley.")

personalizedMessage :: String -> String
personalizedMessage name =
    assembleMessage
        (runReader personalizedSalutation name)
        (runReader personalizedPitch name)
        genericClosing

messageForFranky = personalizedMessage "Franky"

personalizedMessage' :: Reader String String
personalizedMessage' =
    pure assembleMessage
        <*> personalizedSalutation
        <*> personalizedPitch
        <*> pure genericClosing

messageForFranky' = runReader personalizedMessage' "Franky"