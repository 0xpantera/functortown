module Applicative where


database :: [(Integer, String)]
database = [(1, "Mariela"),
            (2, "Franky"),
            (3, "Alonzo"),
            (4, "Melman")]


greetUser :: Integer -> Maybe String
greetUser record =
   mapToMaybe ("Hello, " ++) (lookup record database)


mapToMaybe :: (a -> b) -> Maybe a -> Maybe b
mapToMaybe _ Nothing = Nothing
mapToMaybe function (Just a) = Just (function a)


