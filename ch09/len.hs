module Len where

len :: [a] -> Int
len [] = 0
len (_ : xs) = 1 + len xs

len' :: [a] -> Int
len' [] = 0
len' (x : xs) = 1 + len xs
