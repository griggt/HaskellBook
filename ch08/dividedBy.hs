module DividedBy where

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d     = (count, n)
         | otherwise = go (n - d) d (count + 1)

data DividedResult =
    Result Integer
  | DividedByZero
  deriving (Eq, Show)

-- extra complicated to mirror the rounding behavior of 'div' when
-- working with a negative dividend or divisor and having a remainder
-- Choosing to mimic the behavior of 'quot' is much simpler 
--   (eliminate n == 0 and n < d && sgn < 0 cases)
divBy :: Integral a => a -> a -> DividedResult
divBy num denom = go (abs num) (abs denom) 0 sign
  where go n d count sgn
         | d == 0           = DividedByZero
         | n == 0           = Result (count * sgn)
         | n < d && sgn < 0 = Result (count * sgn - 1)
         | n < d            = Result (count * sgn)
         | otherwise        = go (n - d) d (count + 1) sgn
        sign = toInteger $ (signum num) * (signum denom)
