module Filter where

myFilter :: String -> [String]
myFilter x = filter (not . isArticle) $ words x
  where isArticle = \x -> elem x ["a", "an", "the"]
