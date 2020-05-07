module Rec1 where

-- TODO why is the type constaint (Eq a, Num a) vs (Integral a)??
--   Eq is necessary to pattern match against the argument of 1
--   Num is necessary to perform the arithmetic
--   Integral would introduce additional constraints preventing floating point
sumN :: (Eq a, Num a) => a -> a
sumN 1 = 1
sumN x = x + sumN (x - 1)

multTwo :: (Integral a) => a -> a -> a
multTwo x y
  | y == 0    = 0
  | y < 0     = multTwo (negate x) (negate y)
  | otherwise = x + multTwo x (y - 1)
