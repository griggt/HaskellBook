module Unfolds where

-- exercise #1. use direct recursion
myIterate :: (a -> a) -> a -> [a]
myIterate f z = z : myIterate f (f z)

-- exercise #2. use direct recursion
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f = go []
  where 
    go xs z = case f z of
      Nothing      -> xs
      Just (x, z') -> x : go xs z'

-- exercise #3
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))
-- since iterate generates an infinite list, there is no case
--  where the unfold function should return Nothing
