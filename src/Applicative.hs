module Applicative where

import Control.Applicative
import Data.List (sortBy)
import Data.Function (on)
import Numeric.Natural


data Option a = Some a | None deriving (Eq, Show)

instance Functor Option where
  fmap f None = None
  fmap f (Some x) = Some (f x)
  
instance Applicative Option where
  pure x = Some x
  None <*> _ = None
  _ <*> None = None
  (Some f) <*> (Some x) = Some (f x)
  liftA2 _ None None = None
  liftA2 f _ None = None
  liftA2 f None _ = None
  liftA2 f (Some x) (Some y) = Some (f x y)
  

data User = User { name :: String
                 , surname :: String
                 , age :: Natural
                 , beta :: Bool
                 , admin :: Bool }
  deriving (Eq, Show)


userList :: [User]
userList = [
  User "Julie" "Moronuki" 74 False True,
  User "Chris" "Martin" 25 False True,
  User "Alonzo" "Church" 100 True False,
  User "Alan" "Turing" 99 True False,
  User "Melman" "Fancypants" 0 False False
  ]


fetchUsers :: String -> Maybe [User]
fetchUsers query =
  case query of
    "allusers" -> Just userList
    "betausers" -> Just (filter beta userList)
    "admins" -> Just (filter admin userList)
    _ -> Nothing


readSortMaybe :: String -> Maybe ([User] -> [User])
readSortMaybe ordering =
  case ordering of
    "surname" -> Just (sortBy (compare `on` surname))
    "age" -> Just (sortBy (compare `on` age))
    _ -> Nothing


liftAp :: Maybe (a -> b) -> Maybe a -> Maybe b
liftAp _ Nothing = Nothing
liftAp Nothing _ = Nothing
liftAp (Just f) (Just x) = Just (f x)


sortUsers :: String -> String -> Maybe [User]
sortUsers ordering query =
  liftAp
  (readSortMaybe ordering)
  (fetchUsers query)
