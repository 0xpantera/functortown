{-# LANGUAGE InstanceSigs #-}
module ApplicativeLesson where

import Control.Applicative
import Data.List (sortBy)
import Data.Function (on)
import Numeric.Natural
import Prelude hiding (Either (..))


data Option a = Some a | None deriving (Eq, Show)

instance Functor Option where
  fmap _ None = None
  fmap f (Some x) = Some (f x)

instance Applicative Option where
  pure :: a -> Option a
  pure x = Some x
  (<*>) :: Option (a -> b) -> Option a -> Option b
  None <*> _ = None
  _ <*> None = None
  (Some f) <*> (Some x) = Some (f x)
  liftA2 :: (a -> b -> c) -> Option a -> Option b -> Option c
  liftA2 _ None None = None
  liftA2 _ None _ = None
  liftA2 _ _ None = None
  liftA2 f (Some x) (Some y) = Some (f x y)

data Either a b = Left a | Right b deriving (Eq, Show)

instance Functor (Either a) where
  fmap f (Left x) = Left x
  fmap f (Right y) = Right (f y)

instance Applicative (Either a) where
  pure x = Right x
  (Left x) <*> _ = Left x
  _ <*> (Left y) = Left y
  (Right f) <*> (Right y) = Right (f y)
  liftA2 f (Left x) _ = Left x
  liftA2 f _ (Left y) = Left y
  liftA2 f (Right x) (Right y) = Right (f x y)



data User = User { name :: String
                 , surname :: String
                 , age :: Natural
                 , beta :: Bool
                 , admin :: Bool}
    deriving (Show, Eq)


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
liftAp Nothing _ = Nothing
liftAp _ Nothing = Nothing
liftAp (Just f) (Just x) = Just (f x)


sortUsers :: String -> String -> Maybe [User]
sortUsers ordering query =
  liftAp
  (readSortMaybe ordering)
  (fetchUsers query)

