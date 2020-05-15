{-# LANGUAGE FlexibleInstances #-}

module TooMany where

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
    tooMany (Goats n) = n > 43

-- exercise #1
instance TooMany (Int, String) where
    tooMany (i, _) = i > 41

-- exercise #2
--instance TooMany (Int, Int) where
--    tooMany (x, y) = (x + y) > 42

-- exercise #3
-- note that this definition overlaps with exercise #2 (Int, Int)
--  when a is Int
-- note that we can't define as (x + y) > N because `a` does not
-- necessarily have an instance of an Ord typeclass to use the '>' operator
instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany x || tooMany y
