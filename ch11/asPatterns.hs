module AsPatterns where

import Data.Bool
import Data.Char
import Data.List

-- exercise #1
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = True
isSubseqOf [] _  = True
isSubseqOf _ []  = False
isSubseqOf ax@(x:xs) (y:ys)
  | x == y    = isSubseqOf xs ys
  | otherwise = isSubseqOf ax ys

-- exercise #2
capitalizeWords :: String -> [(String, String)]
capitalizeWords x = zip ws (map cap ws)
    where 
        ws = words x
        cap (x:xs) = (toUpper x) : xs

--- Language exercises

-- exercise #1
capitalizeWord :: String -> String
capitalizeWord ""     = ""
capitalizeWord (x:xs) = (toUpper x) : xs

-- exercise #2
capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . cap True . words
  where 
    cap _    []     = []
    cap flag (x:xs) = w : (cap capNext xs)
      where 
          w = bool x (capitalizeWord x) flag
          capNext = '.' == last x
