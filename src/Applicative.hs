module Applicative where


database :: [(Integer, String)]
database = [(1, "Mariela"),
            (2, "Franky"),
            (3, "Alonzo"),
            (4, "Melman")]


greetUser :: Integer -> Maybe String
greetUser record =
   mapToMaybe ("Hello, " ++) (lookup record database)


mapToMaybe :: (String -> String) ->
             (Maybe String -> Maybe String)
mapToMaybe _ Nothing = Nothing
mapToMaybe function (Just string) =
  Just (function string)
