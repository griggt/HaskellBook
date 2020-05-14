module RewriteAsFolds where

import Data.Bool

--exercise #1
myOr :: [Bool] -> Bool
myOr = foldr
        (\a b -> 
            if a == True
            then True
            else b) False

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

-- exercise #2
myAny :: (a-> Bool) -> [a] -> Bool
myAny f = foldr
            (\a b ->
                if f a
                then True
                else b) False

myAny' :: (a-> Bool) -> [a] -> Bool
myAny' f = foldr ((||) . f) False
-- TODO is this as point free as can be? we still have 'f'
--  NO! we can do better.  see https://dev.to/deciduously/know-when-to-fold-em-1466

-- exercise #3
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr
            (\a b ->
                if a == x
                then True
                else b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = foldr ((||) . (==x)) False

-- using any
myElem'' :: Eq a => a -> [a] -> Bool
myElem'' x = any (==x)

-- exercise #4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- exercise #5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myMap' :: (a -> b) -> [a] -> [b]
myMap' f = flip foldr [] ((:) . f)

myMap'' :: (a -> b) -> [a] -> [b]
myMap'' = flip foldr [] . ((:) .)

-- exercise #6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr
                (\x y ->
                    if f x
                    then x:y
                    else y) []

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f = foldr (\x y -> bool y (x:y) (f x)) []
-- TODO reduce further to point-free style

-- exercise #7
squish :: [[a]] -> [a]
squish = foldr (++) []

-- exercise #8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr 
               (\x y ->
                   (f x) ++ y) []

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = flip foldr [] ((++) . f)

squishMap'' :: (a -> [b]) -> [a] -> [b]
squishMap'' = flip foldr [] . ((++) .)

-- exercise #9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap'' id

-- exercise #10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr
                    (\x y ->
                        if f x y == GT
                        then x
                        else y) (last xs) xs

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f xs = foldr (\x y -> bool y x (f x y == GT)) (last xs) xs

-- TODO: using foldl1 would permit omitting the 'xs' parameter
myMaximumBy'' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy'' f xs = foldl max' (head xs) xs 
                    where
                        max' x y = case f x y of 
                                    GT -> x
                                    _  -> y

-- exercise #11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr
                    (\x y ->
                        if f x y == LT
                        then x
                        else y) (last xs) xs

myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' f xs = foldl min' (head xs) xs
                    where
                        min' x y = case f x y of
                                    LT -> x
                                    _  -> y
