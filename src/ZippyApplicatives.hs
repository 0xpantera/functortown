{-# LANGUAGE InstanceSigs #-}
module ZippyApplicatives where

import Control.Applicative hiding (ZipList (..))


zipFunc :: [a] -> [b] -> [(a,b)]
zipFunc [] _ = []
zipFunc _ [] = []
zipFunc (x:xs) (y:ys) = (x,y) : zipFunc xs ys

zipApp :: [(a -> b)] -> [a] -> [b]
zipApp [] _ = []
zipApp _ [] = []
zipApp (f:fs) (x:xs) = f x : zipApp fs xs

zipApply :: [(a -> b)] -> [a] -> [b]
zipApply xs ys = fmap (\(f, x) -> f x) (zip xs ys)

zipLift2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipLift2 _ [] _ = []
zipLift2 _ _ [] = []
zipLift2 f (x:xs) (y:ys) = f x y : zipLift2 f xs ys

newtype ZipList a = ZipList { getZipList :: [a]}
    deriving (Show)

instance Functor ZipList where
    fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap f (ZipList xs) = ZipList  $ fmap f xs

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    liftA2 f (ZipList xs) (ZipList ys) = ZipList $ zipWith f xs ys
    ZipList _ <*> ZipList [] = ZipList []
    ZipList [] <*> ZipList _ = ZipList []
    ZipList (f:fs) <*> ZipList (x:xs) =
        ZipList  ((f x) : (getZipList (ZipList fs <*> ZipList xs)))

