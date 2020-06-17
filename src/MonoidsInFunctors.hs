module MonoidsInFunctors where

import Control.Applicative
import Data.Monoid


data Person = Person {
    name :: String
  , pl   :: String 
} deriving (Eq, Show)

nonEmpty :: String -> Either String String
nonEmpty str =
    case null str of
        True -> Left "Error: Empty string."
        False -> Right str


mkPerson :: String -> String -> Either String Person
mkPerson name' pl' =  
    Person <$> (nonEmpty name')
           <*> (nonEmpty pl')

data Tuple a b = Tuple a b

instance Functor (Tuple a) where
  fmap func (Tuple x y) = Tuple x (func y)

instance Monoid a => Applicative (Tuple a) where
  pure x = Tuple mempty x
  (Tuple x func) <*> (Tuple y z) = Tuple (x <> y) (func z)
  liftA2 func (Tuple x z) (Tuple y z') = Tuple (x <> y) (func z z')