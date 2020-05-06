module MatchTypes where
    
import Data.List (sort)

-- question 1
i :: Num a => a
--i :: a
i = 1

-- question 2
f :: Float
--f :: Num a => a
f = 1.0
-- question 3
--f' :: Float
f' :: Fractional a => a
f' = 1.0

-- question 4
--f'' :: Float
f'' :: RealFrac a => a
f'' = 1.0

-- question 5
--freud :: a -> a
freud :: Ord a => a -> a
freud x = x

-- question 6
--freud' :: a -> a
freud' :: Int -> Int
freud' x = x

-- TODO why do the replacements for 7 and 8 fail? 
--  Is is because we cannot bind the Int to something more generic,
--  as it is rigid and not polymorphic like a numeric literal would be?

-- question 7
myX = 1 :: Int

sigmund :: Int -> Int
--sigmund :: a -> a
sigmund x = myX

-- question 8
sigmund' :: Int -> Int
--sigmund' :: Num a => a -> a
sigmund' x = myX

-- question 9
--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- question 10
--young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- question 11
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)
