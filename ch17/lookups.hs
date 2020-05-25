module Lookups where

import Control.Applicative
import Data.List (elemIndex)


-- Add the following to make the expressions type check:
--   pure, <$>, <*>

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- zip :: [a] -> [b] -> [(a, b)]

-- #1
added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])


-- #2
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
-- tupled = (<*>) (fmap (,) y) z
-- tupled = liftA2 (,) y z


-- #3
x3 :: Maybe Int
x3 = elemIndex 3 [1, 2, 3, 4, 5]

y3 :: Maybe Int
y3 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x3 <*> y3
-- maxed = (<*>) (fmap max' x3) y3
-- maxed = liftA2 max' x3 y3

-- #4
xs = [1, 2, 3]
ys = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys

summed :: Maybe Integer
-- summed = sum $ (,) x4 y4  -- TODO finish this. can it actually perform summation?
summed = fmap sum ((,) <$> x4 <*> y4) -- type checks, but returns snd, not sum