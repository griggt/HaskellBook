module Zip where

zip' :: [a] -> [b] -> [(a, b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ []          = []
zipWith' _ [] _          = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys


zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)
