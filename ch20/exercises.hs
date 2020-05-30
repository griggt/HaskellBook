module Exercises where

import Data.Monoid (Sum(..), Product(..))

-------------------------------------------------------------
-- exercise #1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-------------------------------------------------------------
-- exercise #2
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-------------------------------------------------------------
-- exercise #3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' p = foldr (\x acc -> acc || x == p) False

-------------------------------------------------------------
-- exercise #4
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing where
    f x Nothing    = Just x
    f x (Just acc)
      | x < acc    = Just x
      | otherwise  = Just acc

-------------------------------------------------------------
-- exercise #5
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f Nothing where
    f x Nothing    = Just x
    f x (Just acc)
      | x > acc    = Just x
      | otherwise  = Just acc

-------------------------------------------------------------
-- exercise #6
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

-------------------------------------------------------------
-- exercise #7
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ c -> c + 1) 0

-------------------------------------------------------------
-- exercise #8
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-------------------------------------------------------------
-- exercise #9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-------------------------------------------------------------
-- exercise #10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x m -> (f x) <> m) mempty
-- also      foldr (mappend . f) mempty
