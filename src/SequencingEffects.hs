{-# LANGUAGE DeriveFunctor #-}
module SequencingEffects where

import Control.Applicative (Applicative (..))
import qualified Data.List as List


newtype Parser a = Parser (String -> Maybe (String, a))
    deriving Functor

instance Applicative Parser where
    pure x = Parser (\str -> Just (str, x))
    liftA2 = parserLiftA2

parserLiftA2 :: (a -> b -> c)
                -> Parser a -> Parser b
                -> Parser c
parserLiftA2 f (Parser p1) (Parser p2) =
    Parser $ \str ->
        do
            (str', x) <- p1 str
            (str'', y) <- p2 str'
            Just (str'', f x y)

parseMaybe :: Parser a -> String -> Maybe a
-- String -> Maybe (String, p)
parseMaybe (Parser p) str =
    case (p str) of
        Just ([], x) -> Just x
        Just _ -> Nothing
        Nothing -> Nothing

exact :: String -> Parser String
exact x = Parser $ \str ->
    case (List.stripPrefix x str) of
        Just str' -> Just (str', x)
        Nothing -> Nothing

anythingBut :: Char -> Parser String
anythingBut c = Parser $ \str ->
    let (match, remainder) = List.span (/= c) str
    in Just (remainder, match)