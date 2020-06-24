{-# LANGUAGE InstanceSigs #-}
module ApplicativesCompose where

import Control.Applicative
import Data.List (sort)
import Data.Char


alphabetize :: String -> String -> [String]
alphabetize name1 name2 = sort [name1, name2]

alphabetizeMaybe :: String -> String -> Maybe [String]
alphabetizeMaybe name1 name2 =
    case (all isAlpha name1) && (all isAlpha name2) of
        True -> Just (alphabetize name1 name2)
        False -> Nothing

alphabetizeIO :: IO (Maybe [String])
alphabetizeIO = 
    pure alphabetizeMaybe <*> getLine <*> getLine

newtype MaybeList a = MaybeList (Maybe [a])
    deriving Show

instance Functor MaybeList where
    fmap :: (a -> b) -> MaybeList a -> MaybeList b
    fmap f (MaybeList xs) = MaybeList $ fmap (fmap f) xs
    -- fmap f (MaybeList Nothing) = MaybeList Nothing
    -- fmap f (MaybeList (Just xs)) = MaybeList $ Just (fmap f xs)

instance Applicative MaybeList where
    pure :: a -> MaybeList a
    pure x = MaybeList (Just [x])

    liftA2 :: (a -> b -> c) -> MaybeList a -> MaybeList b -> MaybeList c
    liftA2 f (MaybeList Nothing) (MaybeList _) = MaybeList Nothing
    liftA2 f (MaybeList _) (MaybeList Nothing) = MaybeList Nothing
    liftA2 f (MaybeList (Just xs)) (MaybeList (Just ys)) = MaybeList $ Just (liftA2 f xs ys)
    -- liftA2 f (MaybeList x) (MaybeList y) = MaybeList $ liftA2 (liftA2 f) x y

    (<*>) :: MaybeList (a -> b) -> MaybeList a -> MaybeList b
    (MaybeList Nothing) <*> (MaybeList _) = MaybeList Nothing
    (MaybeList _) <*> (MaybeList Nothing) = MaybeList Nothing
    (MaybeList (Just fs)) <*> (MaybeList (Just xs)) = MaybeList $ Just (fs <*> xs)


newtype ReaderIO env a = ReaderIO (env -> IO a)

runReaderIO :: ReaderIO env a -> env -> IO a
runReaderIO (ReaderIO f) env = f env

instance Functor (ReaderIO env) where
    fmap :: (a -> b) -> ReaderIO env a -> ReaderIO env b
    fmap f (ReaderIO g) = ReaderIO $ \env -> fmap f (g env)

instance Applicative (ReaderIO env) where
    pure :: a -> ReaderIO env a
    pure x = ReaderIO (\_env -> pure x)

    liftA2 :: (a -> b -> c) -> ReaderIO env a -> ReaderIO env b -> ReaderIO env c
    liftA2 f (ReaderIO g) (ReaderIO h) = ReaderIO $ \env -> liftA2 f (g env) (h env)
    --liftA2 f (ReaderIO g) (ReaderIO h) = ReaderIO $ \env -> pure f <*> g env <*> h env

data Order = Alphabetical | Forward | Reverse

arrange :: Order -> String -> String -> [String]
arrange Forward x y = [x, y]
arrange Reverse x y = [y, x]
arrange Alphabetical x y = sort [x, y]

data LineLimit = NoLimit | MaxLength Int

applyLineLimit :: LineLimit -> String -> String
applyLineLimit NoLimit str = str
applyLineLimit (MaxLength n) str = take n str

data Config =
    Config { configOrder :: Order,
             configLineLimit :: LineLimit }

arrange' :: Config -> String -> String -> [String]
arrange' config = arrange (configOrder config)

getLine' :: Config -> IO String
getLine' config = 
    applyLineLimit (configLineLimit config)
        <$> getLine

fn :: (env -> a) -> ReaderIO env a
fn f = ReaderIO (\env -> pure (f env))

getAndArrange' :: Config -> IO [String]
getAndArrange' = runReaderIO (fn arrange' <*> ReaderIO getLine' <*> ReaderIO getLine')

newtype ReaderT env f a =
    ReaderT (env -> f a)

