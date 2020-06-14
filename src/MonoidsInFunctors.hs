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

